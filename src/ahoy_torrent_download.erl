-module(ahoy_torrent_download).

-export([start_link/3,
         completed_piece/3]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(HEARTBEAT, 500).
-define(NUM_DOWNLOADS, 15).

-type torrent() :: pid().
-type download() :: {ahoy_piece:piece_index(), ahoy_piece:piece()}.
-type downloads() :: list(download()).

-record(state, {torrent :: torrent(),
                dl_select :: ahoy_download_select:ref(),
                create_piece_dl,
                downloads :: downloads()}).

start_link(Torrent, DlSelect, CreatePieceDl) ->
    gen_server:start_link(?MODULE, [Torrent, DlSelect, CreatePieceDl], []).

%% @doc Notify that download of piece is completed.
-spec completed_piece(pid(), ahoy_piece:piece_index(), ahoy_piece:raw_piece()) -> ok.
completed_piece(Pid, PieceIndex, RawPiece) ->
    gen_server:cast(Pid, {completed, PieceIndex, RawPiece}).

init([Torrent, DlSelect, CreatePieceDl]) ->
    State = #state{
        torrent = Torrent,
        dl_select = DlSelect,
        create_piece_dl = CreatePieceDl,
        downloads = []
    },
    heartbeat(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(heartbeat, State) ->
    heartbeat(),
    update(),
    {noreply, State};
handle_cast(update, State=#state{downloads=Downloads,
                                 dl_select=DlSelect,
                                 create_piece_dl=CreatePieceDl}) ->
    N = length(Downloads),
    AvailableSlots = ?NUM_DOWNLOADS - N,
    PeerSelections = ahoy_download_select:select(DlSelect, AvailableSlots),
    Downloads2 = Downloads ++ create_downloads(PeerSelections, CreatePieceDl),
    State2 = State#state{downloads=Downloads2},
    {noreply, State2};
handle_cast({completed, PieceIndex, RawPiece}, State=#state{torrent=Torrent,
                                                                     downloads=Downloads}) ->
    ahoy_torrent:write_raw_piece(Torrent, PieceIndex, RawPiece),
    Downloads2 = lists:keydelete(PieceIndex, 1, Downloads),
    State2 = State#state{downloads=Downloads2},
    update(),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

heartbeat() ->
    timer:apply_after(?HEARTBEAT, gen_server, cast, [self(), heartbeat]).

update() ->
    gen_server:cast(self(), update).

%% Create piece download processes from peer matches.
create_downloads(PeerSelections, CreatePieceDl) ->
    [create_download(PieceIndex, Peer, CreatePieceDl) || {PieceIndex, Peer} <- PeerSelections].

create_download(PieceIndex, Peer, CreatePieceDl) ->
    {ok, PieceDownload} = CreatePieceDl(self(), PieceIndex, Peer),
    {PieceIndex, PieceDownload}.
