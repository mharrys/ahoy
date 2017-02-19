%%% @doc The responsibility of this module is to implement the peer wire
%%% protocol.
%%%
%%% Once the connection is established and handshake is received the exchange
%%% of peer messages begins with a remote peer. The module will keep the
%%% connection alive for as long as the peer is alive.
%%% @end
-module(ahoy_peer).

-behaviour(gen_fsm).

-export([start_link/5,
         connected/1,
         peer_message/2,
         download/5,
         bitfield/1]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([connecting/2,
         handshake/2,
         exchange/2]).

-include_lib("ahoy_block.hrl").

-type peer() :: pid().
-type peer_conn() :: pid().
-type info_hash() :: binary().
-type piece_stat() :: pid().
-type piece_download() :: pid().
-type piece_index() :: non_neg_integer().
-type bitfield() :: pid().
-type download_key() :: {piece_index(), block_offset()}.
-type download() :: {download_key(), piece_download(), block_size()}.
-type downloads() :: list(download()).
-type pending() :: list(download()).

-record(state, {conn :: peer_conn(),
                info_hash :: info_hash(),
                stat :: piece_stat(),
                client_bitfield :: bitfield(),
                remote_bitfield :: bitfield(),
                remote_bitfield_recv = false :: boolean(),
                choke = true :: boolean(),
                interested = false :: boolean(),
                downloads = [] :: downloads(),
                pending = [] :: pending()}).

start_link(Address, InfoHash, Stat, ClientBitfield, RemoteBitfield) ->
    gen_fsm:start_link(?MODULE, [Address, InfoHash, Stat, ClientBitfield, RemoteBitfield], []).

%% @doc Move from connecting to connected state with remote peer.
-spec connected(peer()) -> ok.
connected(Ref) ->
    gen_fsm:send_event(Ref, connected).

%% @doc Receive new message from remote peer.
peer_message(Ref, Message) ->
    gen_fsm:send_event(Ref, Message).

%% @doc Request to download block in piece from remote peer.
-spec download(peer(), piece_download(), piece_index(), block_index(), block_size()) -> ok.
download(Ref, From, PieceIndex, BlockIndex, BlockSize) ->
    BlockOffset = BlockIndex * ?BLOCK_SIZE,
    Download = {{PieceIndex, BlockOffset}, From, BlockSize},
    gen_fsm:send_event(Ref, {download, Download}).

%% @doc Return bitfield process. 
-spec bitfield(peer()) -> bitfield().
bitfield(Ref) ->
    gen_fsm:sync_send_all_state_event(Ref, bitfield).

init([Address, InfoHash, Stat, ClientBitfield, RemoteBitfield]) ->
    case ahoy_peer_conn:start_link(self(), Address) of
        {ok, Conn} ->
            State = #state{
                conn = Conn,
                info_hash = InfoHash,
                stat = Stat,
                client_bitfield = ClientBitfield,
                remote_bitfield = RemoteBitfield
            },
            {ok, connecting, State};
        _ ->
            {stop, "Socket connection failed"}
    end.

connecting(connected, State=#state{conn=Conn, info_hash=InfoHash}) ->
    heartbeat(),
    ahoy_peer_conn:send_handshake(Conn, InfoHash),
    {next_state, handshake, State};
connecting(_Event, State) ->
    {stop, "Expected connected", State}.

handshake({handshake, InfoHash, <<_PeerId:20/binary>>}, State=#state{conn=Conn,
                                                                     info_hash=InfoHash,
                                                                     client_bitfield=Bitfield}) ->
    ahoy_peer_conn:send_bitfield(Conn, Bitfield),
    {next_state, exchange, State};
handshake({handshake, _InfoHash, _PeerId}, State) ->
    {stop, "Invalid handshake", State};
handshake(_Event, State) ->
    {stop, "Expected handshake", State}.

%% Peer wire protocol messages
exchange(keep_alive, State) ->
    % ignore
    {next_state, exchange, State};
exchange(choke, State) ->
    % no longer allowed to send requests
    State2 = State#state{choke=true},
    {next_state, exchange, State2};
exchange(unchoke, State=#state{conn=Conn, pending=Pending, downloads=Downloads}) ->
    % allowed to send requests
    Downloads2 = send_pending(Conn, Downloads, Pending),
    State2 = State#state{choke=false, interested=false, pending=[], downloads=Downloads2},
    {next_state, exchange, State2};
exchange({have, Index}, State=#state{remote_bitfield=RemoteBitfield, stat=Stat}) ->
    % remote peer received new piece or lazy update of bitfield
    ahoy_bitfield:set(RemoteBitfield, Index),
    ahoy_piece_stat:new_piece(Stat, Index),
    {next_state, exchange, State};
exchange({bitfield, Bitfield}, State=#state{remote_bitfield=RemoteBitfield,
                                            remote_bitfield_recv=false,
                                            stat=Stat}) ->
    % current bitfield of remote peer, should only be recieved once
    ahoy_bitfield:replace(RemoteBitfield, Bitfield),
    ahoy_piece_stat:new_bitfield(Stat, Bitfield),
    State2 = State#state{remote_bitfield_recv=true},
    {next_state, exchange, State2};
exchange({block, PieceIndex, BlockOffset, BlockData}, State=#state{downloads=Downloads}) ->
    % download request completed, send back to process that requested it
    Downloads2 = complete_download(PieceIndex, BlockOffset, BlockData, Downloads),
    State2 = State#state{downloads=Downloads2},
    {next_state, exchange, State2};

%% Internal messages
exchange(heartbeat, State=#state{conn=Conn}) ->
    heartbeat(),
    ahoy_peer_conn:send_keep_alive(Conn),
    {next_state, exchange, State};
exchange({download, Download}, State=#state{choke=false, downloads=Downloads, conn=Conn}) ->
    % choke inactive, send download request immediately
    {{PieceIndex, BlockOffset}, _, BlockSize} = Download,
    ahoy_peer_conn:send_request(Conn, PieceIndex, BlockOffset, BlockSize),
    Downloads2 = [Download|Downloads],
    State2 = State#state{downloads=Downloads2},
    {next_state, exchange, State2};
exchange({download, Download}, State=#state{choke=true,
                                            pending=Pending,
                                            interested=Interested,
                                            conn=Conn}) ->
    % chocke active, send interested if needed and wait for unchoke
    ahoy_peer_conn:send_interested(Conn, Interested),
    Pending2 = [Download|Pending],
    State2 = State#state{pending=Pending2, interested=true},
    {next_state, exchange, State2};
exchange(_Event, State) ->
    {stop, "Unknown exchange message", State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(bitfield, _From, StateName, State=#state{remote_bitfield=Bitfield}) ->
    Reply = Bitfield,
    {reply, Reply, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Apply heartbeat after 2 minutes.
heartbeat() ->
    timer:apply_after(120000, gen_fsm, send_event, [self(), heartbeat]).

%% Send pending download requests.
-spec send_pending(peer_conn(), downloads(), pending()) -> downloads().
send_pending(Conn, Downloads, Pending) ->
    lists:foldl(fold_pending(Conn), Downloads, Pending).

%% Closure for folding elements in pending to downloads.
-spec fold_pending(peer_conn()) -> fun((download(), downloads()) -> downloads()).
fold_pending(Conn) ->
    fun(Download, Downloads) ->
        {{PieceIndex, BlockOffset}, _, BlockSize} = Download,
        ahoy_peer_conn:send_request(Conn, PieceIndex, BlockOffset, BlockSize),
        [Download|Downloads]
    end.

%% Pop request from existing request (if pending) and notify about completion.
-spec complete_download(piece_index(), block_offset(), block_data(), downloads()) -> downloads().
complete_download(PieceIndex, BlockOffset, BlockData, Downloads) ->
    Key = {PieceIndex, BlockOffset},
    case lists:keytake(Key, 1, Downloads) of
        {value, {Key, From, _BlockSize}, Downloads2} ->
            BlockIndex = BlockOffset div ?BLOCK_SIZE,
            Block = {BlockIndex, BlockData},
            ahoy_piece_download:completed_block(From, PieceIndex, Block),
            Downloads2;
        false ->
            Downloads
    end.
