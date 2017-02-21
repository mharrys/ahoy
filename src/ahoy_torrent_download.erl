-module(ahoy_torrent_download).

-export([start_link/4,
         completed_piece_download/3]).

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

-record(state, {torrent :: torrent(),
                peer_select :: peer_select(),
                piece_select :: piece_select(),
                factory,
                downloads :: downloads()}).

start_link(Torrent, PeerSelect, PieceSelect, DownloadFactory) ->
    gen_server:start_link(?MODULE, [Torrent, PeerSelect, PieceSelect, DownloadFactory], []).

%% @doc Notify that download of piece is completed.
-spec completed_piece_download(pid(), ahoy_piece:piece_index(), ahoy_piece:raw_piece()) -> ok.
completed_piece_download(Pid, PieceIndex, RawPiece) ->
    gen_server:cast(Pid, {completed_download, PieceIndex, RawPiece}).

init([Torrent, PeerSelect, PieceSelect, DownloadFactory]) ->
    State = #state{
        torrent = Torrent,
        peer_select = PeerSelect,
        piece_select = PieceSelect,
        factory = DownloadFactory,
        downloads = []
    },
    delay_update(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(update, State=#state{downloads=Downloads,
                                 piece_select=PieceSelect,
                                 peer_select=PeerSelect,
                                 factory=Factory}) ->
    delay_update(),
    N = length(Downloads),
    AvailableSlots = ?NUM_DOWNLOADS - N,
    PieceIndices = ahoy_piece_select:reserve(PieceSelect, AvailableSlots),
    {PeerSelections, Remaining} = ahoy_peer_select:select(PeerSelect, PieceIndices),
    ahoy_piece_select:unreserve(PieceSelect, Remaining),
    Downloads2 = Downloads ++ create_downloads(PeerSelections, Factory),
    State2 = State#state{downloads=Downloads2},
    {noreply, State2};
handle_cast({completed_download, PieceIndex, RawPiece}, State=#state{torrent=Torrent,
                                                                     downloads=Downloads}) ->
    ahoy_torrent:write_raw_piece(Torrent, PieceIndex, RawPiece),
    Downloads2 = lists:keydelete(PieceIndex, 1, Downloads),
    State2 = State#state{downloads=Downloads2},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

delay_update() ->
    timer:apply_after(?UPDATE_INTERVAL, gen_server, cast, [self(), update]).

%% Create piece download processes from peer matches.
create_downloads(PeerSelections, Factory) ->
    [create_download(PieceIndex, Peer, Factory) || {PieceIndex, Peer} <- PeerSelections].

create_download(PieceIndex, Peer, Factory) ->
    io:format("Begin download of ~p~n", [PieceIndex]),
    {ok, PieceDownload} = Factory(self(), PieceIndex, Peer),
    {PieceIndex, PieceDownload}.
