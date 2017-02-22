%%% @doc The responsibility of this module is to select new downloads.
%%% @end
-module(ahoy_download_select).

-export([start_link/2,
         select/2,
         unselect/2]).

-export_type([ref/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type ref() :: pid().
-type piece_indices() :: list(ahoy_piece:piece_index()).

-record(state, {peer_select,
                piece_select}).

start_link(PeerSelect, PieceSelect) ->
    gen_server:start_link(?MODULE, [PeerSelect, PieceSelect], []).

%% @doc Return at most N new peer selections.
-spec select(ref(), non_neg_integer()) -> ahoy_peer_select:peer_selections().
select(Ref, N) ->
    gen_server:call(Ref, {select, N}).

%% @doc Unselect specified piece indices making them possible to select again.
-spec unselect(ref(), piece_indices() | ahoy_piece:piece_index()) -> ok.
unselect(Ref, PieceIndices) when is_list(PieceIndices) ->
    gen_server:cast(Ref, {unselect, PieceIndices});
unselect(Ref, PieceIndex) ->
    unselect(Ref, [PieceIndex]).

init([PeerSelect, PieceSelect]) ->
    State = #state{peer_select=PeerSelect, piece_select=PieceSelect},
    {ok, State}.

handle_call({select, N}, _From, State=#state{peer_select=PeerSelect,
                                             piece_select=PieceSelect}) ->
    PieceIndices = ahoy_piece_select:reserve(PieceSelect, N),
    {PeerSelections, Remaining} = ahoy_peer_select:select(PeerSelect, PieceIndices),
    ahoy_piece_select:unreserve(PieceSelect, Remaining),
    Reply = PeerSelections,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({unselect, PieceIndices}, State=#state{piece_select=PieceSelect}) ->
    ahoy_piece_select:unreserve(PieceSelect, PieceIndices),
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
