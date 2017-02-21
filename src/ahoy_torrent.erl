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
                file_writer,
                bitfield,
                piece_stat,
                piece_select,
                peer_select,
                torrent_download,
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
    PieceFactory = ahoy_piece:factory(Length, PieceCount, PieceLength),
    DownloadFactory = ahoy_piece_download:factory(PieceFactory),

    {ok, FileWriter} = ahoy_file_writer:start_link(Name),
    {ok, Bitfield} = ahoy_bitfield:start_link(PieceCount),
    {ok, PieceStat} = ahoy_piece_stat:start_link(PieceCount),
    {ok, PieceSelect} = ahoy_piece_select:start_link(Bitfield, PieceStat),
    {ok, PeerSelect} = ahoy_peer_select:start_link(),
    {ok, TorrentDownload} = ahoy_torrent_download:start_link(
        self(),
        PeerSelect,
        PieceSelect,
        DownloadFactory
    ),
    {ok, PeerSup} = ahoy_peer_sup:start_link(),
    {ok, Tracker} = ahoy_tracker:start_link(self(), Meta, ?PORT),
    State = #state{
        meta = Meta,
        file_writer = FileWriter,
        bitfield = Bitfield,
        piece_stat = PieceStat,
        piece_select = PieceSelect,
        peer_select = PeerSelect,
        torrent_download = TorrentDownload,
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
                                                        file_writer=FileWriter,
                                                        piece_select=PieceSelect,
                                                        bitfield=Bitfield}) ->
    Pieces = Meta#metainfo.info#info.pieces,
    PieceLength = Meta#metainfo.info#info.piece_length,
    Position = PieceIndex * PieceLength,
    IsValid = valid_piece(Pieces, PieceIndex, RawPiece),
    IsWritten = ahoy_file_writer:write(FileWriter, Position, RawPiece) =:= ok,
    case IsValid andalso IsWritten of
        true ->
            ahoy_bitfield:set(Bitfield, PieceIndex),
            io:format("Piece ~p completed~n", [PieceIndex]);
        false ->
            ahoy_piece_select:unreserve(PieceSelect, [PieceIndex]),
            io:format("Piece ~p is invalid~n", [PieceIndex])
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
start_peer(Address, #state{meta=Meta,
                           peer_select=Select,
                           piece_stat=Stat,
                           bitfield=Bitfield,
                           peer_sup=PeerSup}) ->
    PieceCount = Meta#metainfo.info#info.piece_count,
    InfoHash = Meta#metainfo.info_hash,
    {ok, RemoteBitfield} = ahoy_bitfield:start_link(PieceCount),
    case ahoy_peer_sup:start_child(PeerSup, Address, InfoHash, Stat, Bitfield, RemoteBitfield) of
        {ok, Peer} ->
            ahoy_peer_select:add_peer(Select, Peer);
        _ ->
            ok
    end.

%% Return true if RawPiece is equal to the piece hash located at PieceIndex.
valid_piece(Pieces, PieceIndex, RawPiece) ->
    HashLen = 20,
    HashIndex = PieceIndex * HashLen,
    <<_:HashIndex/binary, PieceHash:HashLen/binary, _/binary>> = Pieces,
    PieceHash =:= crypto:hash(sha, RawPiece).
