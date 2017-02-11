%%% @doc The piece module tracks the progress of completed blocks for a single
%%% piece.
%%%
%%% The piece starts without any blocks completed. Missing blocks can be
%%% "popped" from the missing queue as "to be downloaded". Any requested
%%% missing blocks are tracked with a pending queue. When a block is
%%% downloaded it can be added as completed iff it exist in the pending queue.
%%%
%%% Once all blocks are completed the piece can be combined into a binary for
%%% hash validation and writing to disk.
%%%
%%% The block size (bytes) should be arbitrarily chosen. Data is assumed to
%%% work as a binary only and not a bitstring.
%%% @end
-module(ahoy_piece).

-export([start_link/1,
         start_link/2,
         pop_missing_block/1,
         add_completed_block/2,
         raw_piece/1]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(BLOCK_SIZE, 16384).

-type block_index() :: non_neg_integer().
-type block_data() :: binary() | atom().
-type block_size() :: non_neg_integer().
-type block() :: {block_index(), block_data()}.

-record(state, {block_size :: non_neg_integer(),
                num_blocks :: non_neg_integer(),
                missing :: list(block()),
                pending :: list(block()),
                completed :: list(block())}).

%% @doc Create a new piece of specified piece length in bytes.
start_link(PieceLength) ->
    start_link(PieceLength, ?BLOCK_SIZE).

%% @doc Create a new piece of specified piece length and block size in bytes
start_link(PieceLength, BlockSize) ->
    gen_server:start_link(?MODULE, [PieceLength, BlockSize], []).

%% @doc Return tuple of the next missing block along with the expected block
%% size. False if there are no more missing blocks.
-spec pop_missing_block(pid()) -> {ok, {block(), block_size()}} | false.
pop_missing_block(Pid) ->
    gen_server:call(Pid, pop_missing_block).

%% @doc Add missing block with the missing data as completed.
-spec add_completed_block(pid(), block()) -> ok.
add_completed_block(Pid, Block) ->
    gen_server:cast(Pid, {add_completed_block, Block}).

%% @doc Return all blocks as a binary. False if not all blocks are completed.
-spec raw_piece(pid()) -> {ok, binary()} | false.
raw_piece(Pid) ->
    gen_server:call(Pid, raw_piece).

init([PieceLength, BlockSize]) ->
    OneOrZero = case PieceLength rem BlockSize =:= 0 of
        true  -> 0;
        false -> 1
    end,
    NumBlocks = (PieceLength div BlockSize) + OneOrZero,
    Missing = [{X, empty} || X <- lists:seq(0, NumBlocks - 1)],
    Pending = [],
    Completed = [],
    State = #state{block_size = BlockSize, num_blocks = NumBlocks,
        missing = Missing, pending = Pending, completed = Completed},
    {ok, State}.

handle_call(pop_missing_block, _From, State=#state{missing=[]}) ->
    Reply = false,
    {reply, Reply, State};
handle_call(pop_missing_block, _From, State=#state{block_size=BlockSize,
        missing=[Block|NewMissing], pending=Pending}) ->
    Reply = {ok, {Block, BlockSize}},
    NewPending = [Block | Pending],
    NewState = State#state{missing=NewMissing, pending=NewPending},
    {reply, Reply, NewState};
handle_call(raw_piece, _From, State=#state{missing=[], pending=[],
        completed=Completed}) ->
    Sorted = lists:keysort(1, Completed),
    Piece = lists:foldl(fun({_, X}, Acc) -> <<Acc/binary, X/binary>> end,
        <<>>, Sorted),
    Reply = {ok, Piece},
    {reply, Reply, State};
handle_call(raw_piece, _From, State) ->
    {reply, false, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({add_completed_block, {_, empty}}, State) ->
    {noreply, State};
handle_cast({add_completed_block, Block}, State=#state{block_size=BlockSize,
        pending=Pending, completed=Completed}) ->
    {Index, Data} = Block,
    true = byte_size(Data) =:= BlockSize,
    NewState = case lists:keyfind(Index, 1, Pending) of
        {Index, empty} ->
            NewPending = lists:keydelete(Index, 1, Pending),
            NewCompleted = [Block | Completed],
            State#state{pending=NewPending, completed=NewCompleted};
        _ ->
            State
    end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
