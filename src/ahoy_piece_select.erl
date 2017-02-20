%%% @doc The responsibility of this module is to select pieces that should be
%%% downloaded next.
%%%
%%% It will not select pieces that are already downloaded, pieces that no
%%% (of the known peers) have or pieces that are reserved (i.e. in progress).
%%% However, it is not guaranteed that the selected pieces have active peers.
%%% @end
-module(ahoy_piece_select).

-export([start_link/2,
         reserve/2,
         unreserve/2]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type piece_stat() :: pid().
-type piece_index() :: non_neg_integer().
-type piece_indices() :: list(piece_index()).

-record(state, {bitfield :: ahoy_bitfield:bitfield(),
                stat :: piece_stat(),
                reserved :: list(piece_index())}).

start_link(Bitfield, PieceStat) ->
    gen_server:start_link(?MODULE, [Bitfield, PieceStat], []).

%% @doc Reserve between [0, N] number of pieces.
-spec reserve(pid(), pos_integer()) -> piece_indices().
reserve(Pid, N) ->
    gen_server:call(Pid, {reserve, N}).

%% @doc Make specified piece indices to be available for another reservation.
-spec unreserve(pid(), piece_indices()) -> ok.
unreserve(Pid, PieceIndices) ->
    gen_server:cast(Pid, {unreserve, PieceIndices}).

init([Bitfield, PieceStat]) ->
    State = #state{bitfield=Bitfield, stat=PieceStat, reserved=[]},
    {ok, State}.

handle_call({reserve, 0}, _From, State) ->
    Reply = [],
    {reply, Reply, State};
handle_call({reserve, N}, _From, State=#state{bitfield=Bitfield, stat=Stat, reserved=Reserved}) ->
    Sorted = ahoy_piece_stat:sorted(Stat),
    Useful = lists:filter(useful(Bitfield, Reserved), Sorted),
    UsefulN = lists:sublist(Useful, 1, N),
    Reply = lists:map(fun({Index, _Count}) -> Index end, UsefulN),
    Reserved2 = Reserved ++ Reply,
    State2 = State#state{reserved=Reserved2},
    {reply, Reply, State2};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({unreserve, []}, State) ->
    {noreply, State};
handle_cast({unreserve, PieceIndices}, State=#state{reserved=Reserved}) ->
    Reserved2 = lists:subtract(Reserved, PieceIndices),
    State2 = State#state{reserved=Reserved2},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Closure filter for checking that there is at least one peer seen to have
%% the piece and that we don't already have downloaded or is reserved.
useful(Bitfield, Reserved) ->
    fun({Index, Count}) ->
        AtLeastOne = Count > 0,
        NotDownloaded = not(ahoy_bitfield:is_set(Bitfield, Index)),
        NotReserved = not(lists:member(Index, Reserved)),
        AtLeastOne andalso NotDownloaded andalso NotReserved
    end.
