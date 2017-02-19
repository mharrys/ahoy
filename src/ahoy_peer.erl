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

-include_lib("ahoy_block_size.hrl").
-include_lib("ahoy_peer_id.hrl").

-type peer_conn() :: pid().
-type info_hash() :: binary().
-type piece_stat() :: pid().
-type piece_download() :: pid().
-type piece_index() :: non_neg_integer().
-type block_index() :: non_neg_integer().
-type block_offset() :: non_neg_integer().
-type block_size() :: non_neg_integer().
-type block_data() :: binary().
-type bitfield() :: pid().
-type choke() :: boolean().
-type interested() :: boolean().
-type download_key() :: {piece_index(), block_offset()}.
-type download() :: {download_key(), piece_download(), block_size()}.
-type downloads() :: list(download()).
-type pending() :: list(download()).

-record(state, {conn :: peer_conn(),
                info_hash :: info_hash(),
                stat :: piece_stat(),
                client_bitfield :: bitfield(),
                remote_bitfield :: bitfield(),
                remote_bitfield_recv :: boolean(),
                choke :: choke(),
                interested :: interested(),
                downloads :: downloads(),
                pending :: pending()}).

start_link(Address, InfoHash, Stat, ClientBitfield, RemoteBitfield) ->
    gen_fsm:start(?MODULE, [Address, InfoHash, Stat, ClientBitfield, RemoteBitfield], []).

%% @doc Move from connecting to connected state with remote peer.
-spec connected(pid()) -> ok.
connected(Pid) ->
    gen_fsm:send_event(Pid, connected).

%% @doc Receive new message from remote peer.
peer_message(Pid, Message) ->
    gen_fsm:send_event(Pid, Message).

%% @doc Request to download block in piece from remote peer.
-spec download(pid(), piece_download(), piece_index(), block_index(), block_size()) -> ok.
download(Pid, From, PieceIndex, BlockIndex, BlockSize) ->
    % Block index is used internal, but byte offsets are used when talking
    % with remote peers. We use ?BLOCK_SIZE since the offset will never be
    % changed, only the last block may be a different size but that only
    % affects the length, not offset.
    BlockOffset = BlockIndex * ?BLOCK_SIZE,
    Download = {{PieceIndex, BlockOffset}, From, BlockSize},
    gen_fsm:send_event(Pid, {download, Download}).

%% @doc Return bitfield process. No guarantee that the bitfield have or will
%% be received from remote peer.
-spec bitfield(pid()) -> bitfield().
bitfield(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, bitfield).

init([Bitfield]) ->
    State = #state{remote_bitfield=Bitfield},
    {ok, connecting, State};
init([Address, InfoHash, Stat, ClientBitfield, RemoteBitfield]) ->
    {IP, Port} = Address,
    case ahoy_peer_conn:start_link(self(), IP, Port) of
        {ok, Conn} ->
            State = #state{
                conn = Conn,
                info_hash = InfoHash,
                stat = Stat,
                client_bitfield = ClientBitfield,
                remote_bitfield = RemoteBitfield,
                remote_bitfield_recv = false,
                choke = true,
                interested = false,
                downloads = [],
                pending = []
            },
            {ok, connecting, State};
        _ ->
            {stop, "Connection failed"}
    end.

connecting(connected, State=#state{conn=Conn, info_hash=InfoHash}) ->
    % io:format("Connected~n", []),
    heartbeat(),
    send_handshake(Conn, InfoHash),
    {next_state, handshake, State};
connecting(_Event, State) ->
    {stop, "Expected connected", State}.

handshake({handshake, InfoHash, <<PeerId:20/binary>>}, State=#state{
        conn=Conn, info_hash=InfoHash, client_bitfield=ClientBitfield}) ->
    % io:format("Handshake received from ~p~n", [PeerId]),
    send_bitfield(Conn, ClientBitfield),
    {next_state, exchange, State};
handshake({handshake, _InfoHash, _PeerId}, State) ->
    {stop, "Invalid handshake", State};
handshake(_Event, State) ->
    {stop, "Expected handshake", State}.

%% Peer wire custom
exchange(heartbeat, State=#state{conn=Conn}) ->
    heartbeat(),
    send_keep_alive(Conn),
    {next_state, exchange, State};
exchange({download, Download}, State=#state{choke=false, downloads=Downloads, conn=Conn}) ->
    % choke inactive, send download request immediately
    send_request(Conn, Download),
    Downloads2 = [Download|Downloads],
    State2 = State#state{downloads=Downloads2},
    {next_state, exchange, State2};
exchange({download, Download}, State=#state{choke=true, pending=Pending,
        interested=Interested, conn=Conn}) ->
    % chocke active, send interested and wait for unchoke
    send_interested(Conn, Interested),
    Pending2 = [Download|Pending],
    State2 = State#state{pending=Pending2, interested=true},
    {next_state, exchange, State2};

%% Peer wire protocol
exchange(keep_alive, State) ->
    % ignore
    % io:format("Keep alive~n", []),
    {next_state, exchange, State};
exchange(choke, State) ->
    % no longer allowed to send downloads
    % io:format("Choke~n", []),
    NewState = State#state{choke=true},
    {next_state, exchange, NewState};
exchange(unchoke, State=#state{conn=Conn, pending=Pending, downloads=Downloads}) ->
    % either remote peer expect us to start downloading or we sent a request that we are interested
    % to start download
    % io:format("Unchoke~n", []),
    NewDownloads = lists:foldl(send_pending(Conn), Downloads, Pending),
    NewState = State#state{choke=false, interested=false, pending=[], downloads=NewDownloads},
    {next_state, exchange, NewState};
exchange({have, Index}, State=#state{remote_bitfield=RemoteBitfield, stat=Stat}) ->
    % remote peer have received new pieces since its bitfield was sent, or choose to send the
    % bitfield in a "lazy" way
    % io:format("Have ~p~n", [Index]),
    ahoy_bitfield:set(RemoteBitfield, Index),
    ahoy_piece_stat:new_piece(Stat, Index),
    {next_state, exchange, State};
exchange({bitfield, Bitfield}, State=#state{remote_bitfield=RemoteBitfield,
        remote_bitfield_recv=false, stat=Stat}) ->
    % remote peer's bitfield received, this should only be recieved once and is optional to send
    % io:format("Bitfield ~p~n", [Bitfield]),
    ahoy_bitfield:replace(RemoteBitfield, Bitfield),
    ahoy_piece_stat:new_bitfield(Stat, Bitfield),
    NewState = State#state{remote_bitfield_recv=true},
    {next_state, exchange, NewState};
exchange({block, PieceIndex, BlockOffset, BlockData}, State=#state{downloads=Downloads}) ->
    % download request completed, send back to process that requested it
    % io:format("Block recieved PieceIndex(~p) BlockOffset(~p)~n", [PieceIndex, BlockOffset]),
    Downloads2 = complete_download(PieceIndex, BlockOffset, BlockData, Downloads),
    State2 = State#state{downloads=Downloads2},
    {next_state, exchange, State2};
exchange(Event, State) ->
    io:format("ahoy_peer: Unknown message ~p~n", Event),
    {next_state, exchange, State}.

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

%% Send handshake to peer.
-spec send_handshake(peer_conn(), info_hash()) -> ok.
send_handshake(Conn, InfoHash) ->
    Handshake = ahoy_peer_message:encode_handshake(InfoHash, ?PEER_ID),
    ahoy_peer_conn:send(Conn, Handshake).

%% Send bitfield if we have at least one piece downloaded.
-spec send_bitfield(peer_conn(), bitfield()) -> ok.
send_bitfield(Conn, Bitfield) ->
    case ahoy_bitfield:is_empty(Bitfield) of
        true ->
            ok;
        false ->
            RawBitfield = ahoy_bitfield:raw_bitfield(Bitfield),
            BitfieldMsg = ahoy_peer_message:encode_bitfield(RawBitfield),
            ahoy_peer_conn:send(Conn, BitfieldMsg)
    end.

%% Send keep alive message.
-spec send_keep_alive(peer_conn()) -> ok.
send_keep_alive(Conn) ->
    ahoy_peer_conn:send(Conn, ahoy_peer_message:encode_keep_alive()).

%% Send interested iff not already sent.
-spec send_interested(peer_conn(), interested()) -> ok.
send_interested(Conn, false) ->
    InterestedMsg = ahoy_peer_message:encode_interested(),
    ahoy_peer_conn:send(Conn, InterestedMsg);
send_interested(_Conn, _Interested) ->
    % already sent
    ok.

%% Send request to download block.
-spec send_request(peer_conn(), download()) -> ok.
send_request(Conn, {{PieceIndex, BlockOffset}, _From, BlockSize}) ->
    Request = ahoy_peer_message:encode_request(PieceIndex, BlockOffset, BlockSize),
    ahoy_peer_conn:send(Conn, Request).

%% Closure for sending elements in pending list which will produce a new downloads list
-spec send_pending(peer_conn()) -> fun((download(), downloads()) -> downloads()).
send_pending(Conn) ->
    fun(Download, Downloads) ->
        send_request(Conn, Download),
        [Download|Downloads]
    end.

%% Pop request from existing request (if pending) and notify about completion.
-spec complete_download(piece_index(), block_offset(), block_data(), downloads()) -> downloads().
complete_download(PieceIndex, BlockOffset, BlockData, Downloads) ->
    Key = {PieceIndex, BlockOffset},
    case lists:keytake(Key, 1, Downloads) of
        {value, {Key, From, _BlockSize}, Downloads2} ->
            % offsets are same for all blocks, including last
            BlockIndex = BlockOffset div ?BLOCK_SIZE,
            Block = {BlockIndex, BlockData},
            ahoy_piece_download:completed_block(From, PieceIndex, Block),
            Downloads2;
        false ->
            Downloads
    end.
