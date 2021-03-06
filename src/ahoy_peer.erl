%%% @doc The responsibility of this module is to implement the peer wire
%%% protocol.
%%%
%%% Once the connection is established and handshake is received the exchange
%%% of peer messages begins with a remote peer. The module will keep the
%%% connection alive for as long as the peer is alive.
%%% @end
-module(ahoy_peer).

-behaviour(gen_fsm).

-export([start_link/4,
         connected/1,
         peer_message/2,
         download/5]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([connecting/2,
         handshake/2,
         exchange/2]).

-type peer() :: pid().
-type peer_conn() :: pid().
-type info_hash() :: binary().
-type download_key() :: {ahoy_piece:piece_index(), ahoy_piece:block_offset()}.
-type download() :: {download_key(), ahoy_piece_download:piece_dl(), ahoy_piece:block_size()}.
-type downloads() :: list(download()).
-type pending() :: list(download()).

-record(state, {conn :: peer_conn(),
                info_hash :: info_hash(),
                bitfield :: ahoy_bitfield:raw_bitfield(),
                peer_activity :: ahoy_peer_activity:ref(),
                choke = true :: boolean(),
                interested = false :: boolean(),
                downloads = [] :: downloads(),
                pending = [] :: pending()}).

start_link(Address, InfoHash, Bitfield, PeerActivity) ->
    gen_fsm:start_link(?MODULE, [Address, InfoHash, Bitfield, PeerActivity], []).

%% @doc Move from connecting to connected state with remote peer.
-spec connected(peer()) -> ok.
connected(Ref) ->
    gen_fsm:send_event(Ref, connected).

%% @doc Receive new message from remote peer.
peer_message(Ref, Message) ->
    gen_fsm:send_event(Ref, Message).

%% @doc Request to download block in piece from remote peer.
-spec download(peer(), ahoy_piece_download:piece_dl(), ahoy_piece:piece_index(), ahoy_piece:block_offset(), ahoy_piece:block_size()) -> ok.
download(Ref, From, PieceIndex, BlockOffset, BlockSize) ->
    Download = {{PieceIndex, BlockOffset}, From, BlockSize},
    gen_fsm:send_event(Ref, {download, Download}).

init([Address, InfoHash, Bitfield, PeerActivity]) ->
    case ahoy_peer_conn:start_link(self(), Address) of
        {ok, Conn} ->
            State = #state{
                conn = Conn,
                info_hash = InfoHash,
                bitfield = Bitfield,
                peer_activity = PeerActivity
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

handshake({handshake, InfoHash, <<PeerId:20/binary>>}, State=#state{conn=Conn,
                                                                    info_hash=InfoHash,
                                                                    bitfield=Bitfield}) ->
    io:format("Hello from ~p~n", [PeerId]),
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
exchange({have, PieceIndex}, State=#state{peer_activity=PeerActivity}) ->
    % remote peer received new piece or lazy update of bitfield
    ahoy_peer_activity:new_piece_index(PeerActivity, self(), PieceIndex),
    {next_state, exchange, State};
exchange({bitfield, RawBitfield}, State=#state{peer_activity=PeerActivity}) ->
    % current bitfield of remote peer, should only be recieved once
    ahoy_peer_activity:new_bitfield(PeerActivity, self(), RawBitfield),
    {next_state, exchange, State};
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
-spec complete_download(ahoy_piece:piece_index(), ahoy_piece:block_offset(), ahoy_piece:block_data(), downloads()) -> downloads().
complete_download(PieceIndex, BlockOffset, BlockData, Downloads) ->
    Key = {PieceIndex, BlockOffset},
    case lists:keytake(Key, 1, Downloads) of
        {value, {Key, From, BlockSize}, Downloads2} ->
            Block = {BlockOffset, BlockSize, BlockData},
            ahoy_piece_download:completed_block(From, PieceIndex, Block),
            Downloads2;
        false ->
            Downloads
    end.
