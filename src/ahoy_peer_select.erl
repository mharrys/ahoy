%%% @doc The responsibility of this module is to select peers to download
%%% pieces from.
%%% @end
-module(ahoy_peer_select).

-export([start_link/1,
         select/2]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type piece_indices() :: list(ahoy_piece:piece_index()).
-type peer_selection() :: {ahoy_piece:piece_index(), ahoy_peer:peer()}.
-type peer_selections() :: list(peer_selection()).

-record(state, {peer_activity :: ahoy_peer_activity:ref()}).

start_link(PeerActivity) ->
    gen_server:start_link(?MODULE, [PeerActivity], []).

%% @doc Select possible peers to download specified piece indices from. All
%% piece indices that could not be matched with a peer are also returned.
-spec select(pid(), piece_indices()) -> {peer_selections(), piece_indices()}.
select(Pid, PieceIndices) ->
    gen_server:call(Pid, {select, PieceIndices}).

init([PeerActivity]) ->
    State = #state{peer_activity=PeerActivity},
    {ok, State}.

handle_call({select, PieceIndices}, _From, State=#state{peer_activity=PeerActivity}) ->
    Activity = ahoy_peer_activity:current_activity(PeerActivity),
    Reply = find_selections(PieceIndices, Activity),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Return tuple of selections and unmatched indices.
-spec find_selections(piece_indices(), ahoy_peer_activity:activity()) -> {peer_selections(), piece_indices()}.
find_selections([], _Activity) ->
    {[], []};
find_selections(PieceIndices, []) ->
    {[], PieceIndices};
find_selections(PieceIndices, Activity) ->
    find_selections(PieceIndices, Activity, [], []).

find_selections([], _Activity, Selections, Unmatched) ->
    {lists:reverse(Selections), lists:reverse(Unmatched)};
find_selections([PieceIndex|PieceIndices], Activity, Selections, Unmatched) ->
    case find_selection(PieceIndex, Activity) of
        false ->
            Unmatched2 = [PieceIndex|Unmatched],
            find_selections(PieceIndices, Activity, Selections, Unmatched2);
        Selection ->
            Selections2 = [Selection|Selections],
            find_selections(PieceIndices, Activity, Selections2, Unmatched)
    end.

-spec find_selection(ahoy_piece:piece_index(), ahoy_peer_activity:activity()) -> peer_selection() | false.
find_selection(_PieceIndex, []) ->
    false;
find_selection(PieceIndex, [{Peer, Bitfield}|Activity]) ->
    case ahoy_bitfield:is_set(Bitfield, PieceIndex) of
        true ->
            {PieceIndex, Peer};
        false ->
            find_selection(PieceIndex, Activity)
    end.
