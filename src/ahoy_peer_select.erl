%%% @doc The responsibility of this module is to select peers to download
%%% pieces from.
%%% @end
-module(ahoy_peer_select).

-export([start_link/0,
         add_peer/2,
         select/2]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type peer() :: pid().
-type peers() :: list(peer()).
-type piece_index() :: non_neg_integer().
-type piece_indices() :: list(piece_index()).
-type peer_selection() :: {piece_index(), peer()}.
-type peer_selections() :: list(peer_selection()).

-record(state, {peers :: peers()}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Add peer as available for selection.
-spec add_peer(pid(), peer()) -> ok.
add_peer(Pid, Peer) ->
    gen_server:cast(Pid, {add_peer, Peer}).

%% @doc Select possible peers to download specified piece indices from. All
%% piece indices that could not be matched with a peer are also returned.
-spec select(pid(), piece_indices()) -> {peer_selections(), piece_indices()}.
select(Pid, PieceIndices) ->
    gen_server:call(Pid, {select, PieceIndices}).

init([]) ->
    State = #state{peers=[]},
    {ok, State}.

handle_call({select, PieceIndices}, _From, State=#state{peers=Peers}) ->
    Reply = find_selections(PieceIndices, Peers),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({add_peer, Peer}, State=#state{peers=Peers}) ->
    Peers2 = [Peer|Peers],
    State2 = State#state{peers=Peers2},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Return tuple of selections and unmatched indices.
-spec find_selections(piece_indices(), peers()) -> {peer_selections(), piece_indices()}.
find_selections([], _Peers) ->
    {[], []};
find_selections(PieceIndices, []) ->
    {[], PieceIndices};
find_selections(PieceIndices, Peers) ->
    find_selections(PieceIndices, Peers, [], []).

find_selections([], _Peers, Selections, Unmatched) ->
    {lists:reverse(Selections), lists:reverse(Unmatched)};
find_selections([PieceIndex|PieceIndices], Peers, Selections, Unmatched) ->
    case find_selection(PieceIndex, Peers) of
        false ->
            Unmatched2 = [PieceIndex|Unmatched],
            find_selections(PieceIndices, Peers, Selections, Unmatched2);
        Selection ->
            Selections2 = [Selection|Selections],
            find_selections(PieceIndices, Peers, Selections2, Unmatched)
    end.

-spec find_selection(piece_index(), peers()) -> peer_selection() | false.
find_selection(_PieceIndex, []) ->
    false;
find_selection(PieceIndex, [Peer|Peers]) ->
    Bitfield = ahoy_peer:bitfield(Peer),
    case ahoy_bitfield:is_set(Bitfield, PieceIndex) of
        true ->
            {PieceIndex, Peer};
        false ->
            find_selection(PieceIndex, Peers)
    end.
