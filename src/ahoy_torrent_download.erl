-module(ahoy_torrent_download).

-export([start_link/4,
         completed_piece_download/3,
         completed_piece_write/3]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(UPDATE_INTERVAL, 250).
-define(NUM_DOWNLOADS, 5).

-type torrent() :: pid().
-type peer_select() :: pid().
-type piece_select() :: pid().
-type download() :: {ahoy_piece:piece_index(), ahoy_piece:piece()}.
-type downloads() :: list(download()).
-type write() :: download().
-type writes() :: list(write()).
-type write_result() :: ok | false.

-record(state, {torrent :: torrent(),
                peer_select :: peer_select(),
                piece_select :: piece_select(),
                piece_length :: ahoy_piece:piece_length(),
                last_piece,
                downloads :: downloads(),
                writes :: writes()}).

start_link(Torrent, PeerSelect, PieceSelect, PieceInfo) ->
    gen_server:start_link(?MODULE, [Torrent, PeerSelect, PieceSelect, PieceInfo], []).

%% @doc Notify that download of piece is completed.
-spec completed_piece_download(pid(), ahoy_piece:piece_index(), ahoy_piece:raw_piece()) -> ok.
completed_piece_download(Pid, PieceIndex, RawPiece) ->
    gen_server:cast(Pid, {completed_download, PieceIndex, RawPiece}).

%% @doc Notify that writing of piece to disk is completed.
-spec completed_piece_write(pid(), ahoy_piece:piece_index(), write_result()) -> ok.
completed_piece_write(Pid, PieceIndex, Result) ->
    gen_server:cast(Pid, {completed_write, PieceIndex, Result}).

init([Torrent, PeerSelect, PieceSelect, PieceInfo]) ->
    {PieceLength, LastPiece} = PieceInfo,
    State = #state{
        torrent = Torrent,
        peer_select = PeerSelect,
        piece_select = PieceSelect,
        piece_length = PieceLength,
        last_piece = LastPiece,
        downloads = [],
        writes = []
    },
    delay_update(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(update, State=#state{downloads=Downloads,
                                 piece_select=PieceSelect,
                                 peer_select=PeerSelect,
                                 piece_length=PieceLength,
                                 last_piece=LastPiece}) ->
    delay_update(),
    N = length(Downloads),
    AvailableSlots = ?NUM_DOWNLOADS - N,
    PieceIndices = ahoy_piece_select:reserve(PieceSelect, AvailableSlots),
    {PeerSelections, Remaining} = ahoy_peer_select:select(PeerSelect, PieceIndices),
    ahoy_piece_select:unreserve(PieceSelect, Remaining),
    Downloads2 = Downloads ++ create_downloads(PeerSelections, PieceLength, LastPiece),
    State2 = State#state{downloads=Downloads2},
    {noreply, State2};
handle_cast({completed_download, PieceIndex, RawPiece}, State=#state{torrent=Torrent,
                                                                     downloads=Downloads,
                                                                     writes=Writes}) ->
    {Downloads3, Writes3} = case lists:keytake(PieceIndex, 1, Downloads) of
        {value, Download, Downloads2} ->
            ahoy_torrent:write_raw_piece(Torrent, PieceIndex, RawPiece),
            Writes2 = [Download|Writes],
            {Downloads2, Writes2};
        false ->
            {Downloads, Writes}
    end,
    State2 = State#state{downloads=Downloads3, writes=Writes3},
    {noreply, State2};
handle_cast({completed_write, PieceIndex, ok}, State=#state{writes=Writes}) ->
    Writes2 = lists:keydelete(PieceIndex, 1, Writes),
    State2 = State#state{writes=Writes2},
    {noreply, State2};
handle_cast({completed_write, PieceIndex, false}, State=#state{writes=Writes,
                                                               piece_select=PieceSelect}) ->
    Writes2 = lists:keydelete(PieceIndex, 1, Writes),
    ahoy_piece_select:unreserve(PieceSelect, [PieceIndex]),
    State2 = State#state{writes=Writes2},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

delay_update() ->
    timer:apply_after(?UPDATE_INTERVAL, gen_server, cast, [self(), update]).

%% Create piece download processes from peer matches.
create_downloads(PeerSelections, PieceLength, LastPiece) ->
    [create_download(PieceIndex, PieceLength, Peer, LastPiece) || {PieceIndex, Peer} <- PeerSelections].

create_download(PieceIndex, PieceLength, Peer, LastPiece) ->
    io:format("Begin download of ~p~n", [PieceIndex]),
    {ok, PieceDownload} = ahoy_piece_download:start_link(self(), PieceIndex, PieceLength, Peer, LastPiece),
    {PieceIndex, PieceDownload}.
