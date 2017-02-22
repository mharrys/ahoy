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

-define(HEARTBEAT, 3600).
-define(KILL_TIME, 10000).
-define(NUM_DOWNLOADS, 50).

-type torrent() :: pid().
-type download() :: {ahoy_piece:piece_index(), ahoy_piece_download:piece_dl(), ahoy_peer:peer()}.
-type downloads() :: list(download()).

-record(state, {torrent :: torrent(),
                dl_select :: ahoy_download_select:ref(),
                create_piece_dl,
                downloads = [] :: downloads(),
                monitors = []}).

start_link(Torrent, DlSelect, CreatePieceDl) ->
    gen_server:start_link(?MODULE, [Torrent, DlSelect, CreatePieceDl], []).

%% @doc Notify that download of piece is completed.
-spec completed_piece(pid(), ahoy_piece:piece_index(), ahoy_piece:raw_piece()) -> ok.
completed_piece(Pid, PieceIndex, RawPiece) ->
    gen_server:cast(Pid, {completed, PieceIndex, RawPiece}).

init([Torrent, DlSelect, CreatePieceDl]) ->
    State = #state{torrent = Torrent, dl_select = DlSelect, create_piece_dl = CreatePieceDl},
    heartbeat(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(heartbeat, State=#state{downloads=Downloads, dl_select=DlSelect}) ->
    heartbeat(),
    KillFold = fun({PieceIndex, PieceDl, _Peer}, Acc) ->
        case ahoy_piece_download:time_since_request(PieceDl) >= ?KILL_TIME of
            true ->
                ahoy_download_select:unselect(DlSelect, PieceIndex),
                stop_download(PieceIndex, Acc);
            false ->
                Acc
        end
    end,
    State2 = lists:foldl(KillFold, State, Downloads),
    update(),
    {noreply, State2};
handle_cast(update, State=#state{downloads=Downloads,
                                 monitors=Monitors,
                                 dl_select=DlSelect,
                                 create_piece_dl=CreatePieceDl}) ->
    N = length(Downloads),
    AvailableSlots = ?NUM_DOWNLOADS - N,
    PeerSelections = ahoy_download_select:select(DlSelect, AvailableSlots),
    NewPeers = lists:map(fun({_, Peer}) -> Peer end, PeerSelections),
    Monitors2 = monitor_new_peers(NewPeers, Monitors),
    Downloads2 = Downloads ++ create_downloads(PeerSelections, CreatePieceDl),
    State2 = State#state{downloads=Downloads2, monitors=Monitors2},
    {noreply, State2};
handle_cast({completed, PieceIndex, RawPiece}, State=#state{torrent=Torrent}) ->
    ahoy_torrent:write_raw_piece(Torrent, PieceIndex, RawPiece),
    State2 = stop_download(PieceIndex, State),
    update(),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info({'DOWN', _MonitorRef, process, Peer, _Reason}, State=#state{dl_select=DlSelect,
                                                                        downloads=Downloads,
                                                                        monitors=Monitors}) ->
    Downloads2 = remove_peer(Peer, DlSelect, Downloads),
    Monitors2 = lists:keydelete(Peer, 1, Monitors),
    State2 = State#state{downloads=Downloads2, monitors=Monitors2},
    {noreply, State2};
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

%% Monitor unseen peers.
monitor_new_peers(Peers, Monitors) ->
    % remove duplicate peers
    Peers2 = sets:from_list(Peers),
    % remove peers already in monitors
    Peers3 = sets:filter(fun(Peer) -> not(lists:keymember(Peer, 1, Monitors)) end, Peers2),
    % monitor remaining peers
    Peers4 = sets:to_list(Peers3),
    Monitors2 = lists:map(fun(Peer) -> {Peer, monitor(process, Peer)} end, Peers4),
    % merge
    Monitors ++ Monitors2.

%% Create piece download processes from peer matches.
create_downloads(PeerSelections, CreatePieceDl) ->
    [create_download(PieceIndex, Peer, CreatePieceDl) || {PieceIndex, Peer} <- PeerSelections].

create_download(PieceIndex, Peer, CreatePieceDl) ->
    {ok, PieceDownload} = CreatePieceDl(self(), PieceIndex, Peer),
    {PieceIndex, PieceDownload, Peer}.

%% Stop download of piece.
stop_download(PieceIndex, State=#state{downloads=Downloads, monitors=Monitors}) ->
    case lists:keytake(PieceIndex, 1, Downloads) of
        {value, {PieceIndex, PieceDl, Peer}, Downloads2} ->
            ahoy_piece_download:stop(PieceDl),
            Monitors2 = maybe_stop_monitor(Peer, Downloads2, Monitors),
            State#state{downloads=Downloads2, monitors=Monitors2}
    end.

%% Stop monitoring if peer does not have any active downloads.
maybe_stop_monitor(Peer, Downloads, Monitors) ->
    IsPeerActive = lists:keymember(Peer, 3, Downloads),
    stop_monitor(Peer, Monitors, IsPeerActive).

stop_monitor(Peer, Monitors, false) ->
    case lists:keytake(Peer, 1, Monitors) of
        {value, {Peer, MonitorRef}, Monitors2} ->
            demonitor(MonitorRef),
            Monitors2
    end;
stop_monitor(_Peer, Monitors, true) ->
    Monitors.

%% Remove peer from downloads.
-spec remove_peer(ahoy_peer:peer(), ahoy_download_select:ref(), downloads()) -> downloads().
remove_peer(Peer, DlSelect, Downloads) ->
    remove_peer(Peer, DlSelect, Downloads, []).

remove_peer(_Peer, _DlSelect, [], Acc) ->
    Acc;
remove_peer(Peer, DlSelect, [{PieceIndex, PieceDl, Peer}|Downloads], Acc) ->
    ahoy_download_select:unselect(DlSelect, PieceIndex),
    ahoy_piece_download:stop(PieceDl),
    remove_peer(Peer, DlSelect, Downloads, Acc);
remove_peer(Peer, DlSelect, [Download|Downloads], Acc) ->
    remove_peer(Peer, DlSelect, Downloads, [Download|Acc]).
