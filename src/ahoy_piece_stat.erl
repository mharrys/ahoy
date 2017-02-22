%%% @doc The responsibility of this module is to gather statistics of remote
%%% peers downloaded pieces i.e. bitfields.
%%% @end
-module(ahoy_piece_stat).

-export([start_link/1,
         add_bitfield/2,
         remove_bitfield/2,
         add_piece_index/2,
         remove_piece_index/2,
         sorted/1]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type piece_count() :: {ahoy_piece:piece_index(), non_neg_integer()}.
-type piece_counts() :: list(piece_count()).

-record(state, {pieces :: piece_counts()}).

start_link(PieceCount) ->
    gen_server:start_link(?MODULE, [PieceCount], []).

%% @doc Increment one where bitfield is set.
-spec add_bitfield(pid(), ahoy_bitfield:raw_bitfield()) -> ok.
add_bitfield(Pid, Bitfield) ->
    gen_server:cast(Pid, {add_bitfield, Bitfield}).

%% @doc Decrement one where bitfield is set.
-spec remove_bitfield(pid(), ahoy_bitfield:raw_bitfield()) -> ok.
remove_bitfield(Pid, Bitfield) ->
    gen_server:cast(Pid, {remove_bitfield, Bitfield}).

%% @doc Increment one at specified piece index.
-spec add_piece_index(pid(), ahoy_piece:piece_index()) -> ok.
add_piece_index(Pid, PieceIndex) ->
    gen_server:cast(Pid, {add_piece_index, PieceIndex}).

%% @doc Decrement one at specified piece index.
-spec remove_piece_index(pid(), ahoy_piece:piece_index()) -> ok.
remove_piece_index(Pid, PieceIndex) ->
    gen_server:cast(Pid, {remove_piece_index, PieceIndex}).

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

handle_cast({add_bitfield, Bitfield}, State=#state{pieces=Pieces}) ->
    Pieces2 = bitfield_inc(Bitfield, Pieces),
    State2 = State#state{pieces=Pieces2},
    {noreply, State2};
handle_cast({remove_bitfield, Bitfield}, State=#state{pieces=Pieces}) ->
    Pieces2 = bitfield_dec(Bitfield, Pieces),
    State2 = State#state{pieces=Pieces2},
    {noreply, State2};
handle_cast({add_piece_index, PieceIndex}, State=#state{pieces=Pieces}) ->
    Pieces2 = piece_index_inc(PieceIndex, Pieces),
    State2 = State#state{pieces=Pieces2},
    {noreply, State2};
handle_cast({remove_piece_index, PieceIndex}, State=#state{pieces=Pieces}) ->
    Pieces2 = piece_index_dec(PieceIndex, Pieces),
    State2 = State#state{pieces=Pieces2},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Increment where bitfield is set.
bitfield_inc(Bitfield, Pieces) ->
    bitfield_op(Bitfield, Pieces, [], fun inc/1).

%% Decrement where bitfield is set.
bitfield_dec(Bitfield, Pieces) ->
    bitfield_op(Bitfield, Pieces, [], fun dec/1).

bitfield_op(<<>>, [], Acc, _) ->
    lists:reverse(Acc);
bitfield_op(<<0:1, T1/bitstring>>, [{Index, Count}|T2], Acc, Op) ->
    bitfield_op(T1, T2, [{Index, Count} | Acc], Op);
bitfield_op(<<1:1, T1/bitstring>>, [{Index, Count}|T2], Acc, Op) ->
    bitfield_op(T1, T2, [{Index, Op(Count)} | Acc], Op).

%% Increment at piece index.
piece_index_inc(PieceIndex, Pieces) ->
    piece_index_op(PieceIndex, Pieces, [], fun inc/1).

%% Decrement at piece index.
piece_index_dec(PieceIndex, Pieces) ->
    piece_index_op(PieceIndex, Pieces, [], fun dec/1).

piece_index_op(_, [], Acc, _) ->
    lists:reverse(Acc);
piece_index_op(PieceIndex, [{PieceIndex, Count}|T], Acc, Op) ->
    lists:reverse([{PieceIndex, Op(Count)}|Acc]) ++ T;
piece_index_op(PieceIndex, [H|T], Acc, Op) ->
    piece_index_op(PieceIndex, T, [H|Acc], Op).

inc(Count) ->
    Count + 1.

dec(Count) ->
    Count - 1.
