%%% @doc The responsibility of this module is to communicate peer messages
%%% over a TCP socket.
%%% @end
-module(ahoy_peer_conn).

-behaviour(gen_server).

-export([start_link/3,
         send/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(INIT_TIMEOUT, 3600).
-define(SEND_TIMEOUT, 3600).

-type peer() :: pid().
-type socket() :: gen_tcp:socket().
-type data() :: binary().
-type packet() :: list() | binary().

-record(state, {peer :: peer(),
                socket :: socket(),
                data :: data()}).

start_link(Peer, IP, Port) ->
    gen_server:start(?MODULE, [Peer, IP, Port], [{timeout, ?INIT_TIMEOUT}]).

%% @doc Send message over socket.
-spec send(pid(), packet()) -> ok.
send(Pid, Msg) ->
    gen_server:call(Pid, {send, Msg}).

init([Peer, IP, Port]) ->
    Options = [
        binary,
        {packet, 0},
        {reuseaddr, true},
        {active, once},
        {send_timeout, ?SEND_TIMEOUT}
    ],
    case gen_tcp:connect(IP, Port, Options) of
        {ok, Socket} ->
            ahoy_peer_wire:connected(Peer),
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
    [ahoy_peer_wire:peer_message(Peer, Message) || Message <- Messages],
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
