%%% @doc The responsibility of this module is to gather statistics of remote
%%% peers downloaded pieces i.e. bitfields.
%%% @end
-module(ahoy_piece_stat).

-export([start_link/1,
         new_bitfield/2,
         new_piece/2,
         sorted/1]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type raw_bitfield() :: binary().
-type piece_index() :: non_neg_integer().
-type piece_count() :: {piece_index(), non_neg_integer()}.
-type piece_counts() :: list(piece_count()).

-record(state, {pieces :: piece_counts()}).

start_link(PieceCount) ->
    gen_server:start_link(?MODULE, [PieceCount], []).

%% @doc Count one at set pieces in specified raw bitfield.
-spec new_bitfield(pid(), raw_bitfield()) -> ok.
new_bitfield(Pid, Bitfield) ->
    gen_server:cast(Pid, {bitfield, Bitfield}).

%% @doc Count one at specified piece index.
-spec new_piece(pid(), piece_index()) -> ok.
new_piece(Pid, PieceIndex) ->
    gen_server:cast(Pid, {piece, PieceIndex}).

%% @doc Return sorted list with piece count in ascending order i.e. the first
%% piece is the piece that the least number of peers have.
sorted(Pid) ->
    gen_server:call(Pid, sort).

init([PieceCount]) ->
    Align = round(PieceCount / 8) * 8,
    Pieces = [{Index, 0} || Index <- lists:seq(0, Align - 1)],
    State = #state{pieces=Pieces},
    {ok, State}.

handle_call(sort, _From, State=#state{pieces=Pieces}) ->
    % sort on piece count in ascending order
    Reply = lists:keysort(2, Pieces),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({bitfield, Bitfield}, State=#state{pieces=Pieces}) ->
    Pieces2 = merge(Bitfield, Pieces),
    State2 = State#state{pieces=Pieces2},
    {noreply, State2};
handle_cast({piece, PieceIndex}, State=#state{pieces=Pieces}) ->
    Pieces2 = inc(PieceIndex, Pieces),
    State2 = State#state{pieces=Pieces2},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Merge pieces with raw bitfield i.e. add count where bits are set in bitfield.
-spec merge(raw_bitfield(), piece_counts()) -> piece_counts().
merge(Bitfield, Pieces) ->
    merge(Bitfield, Pieces, []).

merge(<<>>, [], Acc) ->
    lists:reverse(Acc);
merge(<<0:1, T1/bitstring>>, [{Index, Count}|T2], Acc) ->
    merge(T1, T2, [{Index, Count} | Acc]);
merge(<<1:1, T1/bitstring>>, [{Index, Count}|T2], Acc) ->
    merge(T1, T2, [{Index, Count + 1} | Acc]).

%% Increment count at index.
-spec inc(piece_index(), list(piece_count())) -> list(piece_count()).
inc(PieceIndex, Pieces) ->
    inc(PieceIndex, Pieces, []).

inc(_, [], Acc) ->
    lists:reverse(Acc);
inc(PieceIndex, [{PieceIndex, Count}|T], Acc) ->
    lists:reverse([{PieceIndex, Count + 1}|Acc]) ++ T;
inc(PieceIndex, [H|T], Acc) ->
    inc(PieceIndex, T, [H|Acc]).
