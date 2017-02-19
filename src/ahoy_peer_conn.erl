%%% @doc The responsibility of this module is to communicate peer messages
%%% over a TCP socket.
%%% @end
-module(ahoy_peer_conn).

-behaviour(gen_server).

-export([start_link/2,
         send/2,
         send_handshake/2,
         send_bitfield/2,
         send_keep_alive/1,
         send_interested/2,
         send_request/4]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("ahoy_peer_id.hrl").

-define(INIT_TIMEOUT, 3600).
-define(SEND_TIMEOUT, 3600).

-type peer_conn() :: pid().
-type peer() :: pid().
-type bitfield() :: pid().
-type info_hash() :: binary().
-type socket() :: gen_tcp:socket().
-type data() :: binary().
-type packet() :: list() | binary().

-record(state, {peer :: peer(),
                socket :: socket(),
                data :: data()}).

start_link(Peer, Address) ->
    gen_server:start_link(?MODULE, [Peer, Address], [{timeout, ?INIT_TIMEOUT}]).

%% @doc Send message over socket.
-spec send(peer_conn(), packet()) -> ok.
send(Ref, Msg) ->
    gen_server:call(Ref, {send, Msg}).

%% @doc Send handshake message.
-spec send_handshake(peer_conn(), info_hash()) -> ok.
send_handshake(Ref, InfoHash) ->
    Msg = ahoy_peer_message:encode_handshake(InfoHash, ?PEER_ID),
    send(Ref, Msg).

%% @doc Send bitfield iff at least one piece is downloaded.
-spec send_bitfield(peer_conn(), bitfield()) -> ok.
send_bitfield(Ref, Bitfield) ->
    case ahoy_bitfield:is_empty(Bitfield) of
        true ->
            ok;
        false ->
            RawBitfield = ahoy_bitfield:raw_bitfield(Bitfield),
            Msg = ahoy_peer_message:encode_bitfield(RawBitfield),
            send(Ref, Msg)
    end.

%% @doc Send keep alive message.
-spec send_keep_alive(peer_conn()) -> ok.
send_keep_alive(Ref) ->
    Msg = ahoy_peer_message:encode_keep_alive(),
    send(Ref, Msg).

%% @doc Send interested iff false.
-spec send_interested(peer_conn(), boolean()) -> ok.
send_interested(Ref, false) ->
    Msg = ahoy_peer_message:encode_interested(),
    send(Ref, Msg);
send_interested(_Ref, true) ->
    ok.

%% @doc Send request to download block from piece.
send_request(Ref, PieceIndex, BlockOffset, BlockSize) ->
    Msg = ahoy_peer_message:encode_request(PieceIndex, BlockOffset, BlockSize),
    send(Ref, Msg).

init([Peer, {IP, Port}]) ->
    Options = [
        binary,
        {packet, 0},
        {reuseaddr, true},
        {active, once},
        {send_timeout, ?SEND_TIMEOUT}
    ],
    case gen_tcp:connect(IP, Port, Options) of
        {ok, Socket} ->
            ahoy_peer:connected(Peer),
            {ok, #state{peer=Peer, socket=Socket, data= <<>>}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({send, Msg}, _From, State=#state{socket=Socket}) ->
    ok = gen_tcp:send(Socket, Msg),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Bin}, State=#state{peer=Peer, data=Prev}) ->
    Data = list_to_binary([Prev, Bin]),
    {Messages, Rest} = ahoy_peer_message:decode_messages(Data),
    [ahoy_peer:peer_message(Peer, Message) || Message <- Messages],
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{data=Rest}};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, _Reason}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
