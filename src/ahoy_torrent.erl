%%% @doc The responsibility of this module is to describe one torrent.
%%% @end
-module(ahoy_torrent).

-export([start_link/1,
         update_tracker_peers/2,
         write_raw_piece/3]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("ahoy_address.hrl").
-include_lib("ahoy_metainfo.hrl").
-include_lib("ahoy_port.hrl").

-record(state, {meta,
                file,
                bitfield,
                piece_stat,
                piece_select,
                peer_activity,
                peer_select,
                torrent_dl,
                tracker_progress,
                tracker,
                active_peers,
                peer_sup}).

start_link(Path) ->
    gen_server:start_link(?MODULE, [Path], []).

%% @doc Update peers received form tracker.
-spec update_tracker_peers(pid(), list(address())) -> ok.
update_tracker_peers(Pid, PeerAddresses) ->
    gen_server:cast(Pid, {peers, PeerAddresses}).

%% @doc Write raw piece to disk iff raw piece have the correct hash.
write_raw_piece(Pid, PieceIndex, RawPiece) ->
    gen_server:cast(Pid, {write, PieceIndex, RawPiece}).

init([Path]) ->
    Meta = ahoy_metainfo:new(Path),
    Name = Meta#metainfo.info#info.name,
    Length = Meta#metainfo.info#info.length,
    PieceCount = Meta#metainfo.info#info.piece_count,
    PieceLength = Meta#metainfo.info#info.piece_length,
    CreatePiece = ahoy_piece:factory(Length, PieceCount, PieceLength),
    CreatePieceDl = ahoy_piece_download:factory(CreatePiece),
    CreateBitfield = ahoy_bitfield:factory(PieceCount),
    {ok, File} = ahoy_file:start_link(Name),
    {ok, Bitfield} = CreateBitfield(),
    {ok, PieceStat} = ahoy_piece_stat:start_link(PieceCount),
    {ok, PieceSelect} = ahoy_piece_select:start_link(Bitfield, PieceStat),
    {ok, PeerActivity} = ahoy_peer_activity:start_link(PieceStat, CreateBitfield),
    {ok, PeerSelect} = ahoy_peer_select:start_link(PeerActivity),
    {ok, DlSelect} = ahoy_download_select:start_link(PeerSelect, PieceSelect),
    {ok, TorrentDl} = ahoy_torrent_download:start_link(self(), DlSelect, CreatePieceDl),
    {ok, PeerSup} = ahoy_peer_sup:start_link(),
    {ok, Progress} = ahoy_tracker_progress:start_link(Length),
    {ok, Tracker} = ahoy_tracker:start_link(self(), Progress, Meta, ?PORT),
    State = #state{
        meta = Meta,
        file = File,
        bitfield = Bitfield,
        piece_stat = PieceStat,
        piece_select = PieceSelect,
        peer_activity = PeerActivity,
        peer_select = PeerSelect,
        torrent_dl = TorrentDl,
        tracker_progress = Progress,
        tracker = Tracker,
        active_peers = [],
        peer_sup = PeerSup
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({peers, PeerAddresses}, State=#state{active_peers=ActivePeers}) ->
    Peers2 = lists:filter(fun(Address) -> not(lists:member(Address, ActivePeers)) end, PeerAddresses),
    [start_peer(Address, State) || Address <- Peers2],
    ActivePeers2 = ActivePeers ++ Peers2,
    State2 = State#state{active_peers=ActivePeers2},
    {noreply, State2};
handle_cast({write, PieceIndex, RawPiece}, State=#state{meta=Meta,
                                                        file=File,
                                                        piece_select=PieceSelect,
                                                        tracker_progress=Progress,
                                                        bitfield=Bitfield}) ->
    Pieces = Meta#metainfo.info#info.pieces,
    PieceLength = Meta#metainfo.info#info.piece_length,
    Position = PieceIndex * PieceLength,
    IsValid = valid_piece(Pieces, PieceIndex, RawPiece),
    IsWritten = ahoy_file:write(File, Position, RawPiece) =:= ok,
    case IsValid andalso IsWritten of
        true ->
            ahoy_bitfield:set(Bitfield, PieceIndex),
            ahoy_tracker_progress:downloaded(Progress, byte_size(RawPiece));
        false ->
            ahoy_piece_select:unreserve(PieceSelect, [PieceIndex])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Add started peer process to peer selection process.
start_peer(Address, #state{meta=Meta, peer_activity=Activity, bitfield=Bitfield, peer_sup=Sup}) ->
    InfoHash = Meta#metainfo.info_hash,
    RawBitfield = ahoy_bitfield:raw_bitfield(Bitfield),
    ahoy_peer_sup:start_child(Sup, Address, InfoHash, RawBitfield, Activity).

%% Return true if RawPiece is equal to the piece hash located at PieceIndex.
valid_piece(Pieces, PieceIndex, RawPiece) ->
    HashLen = 20,
    HashIndex = PieceIndex * HashLen,
    <<_:HashIndex/binary, PieceHash:HashLen/binary, _/binary>> = Pieces,
    PieceHash =:= crypto:hash(sha, RawPiece).
