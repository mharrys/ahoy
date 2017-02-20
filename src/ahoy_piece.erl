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
         start_link/3,
         pop_missing_block/1,
         add_completed_block/2,
         raw_piece/1]).

-export_type([piece/0,
              piece_index/0,
              piece_length/0,
              raw_piece/0,
              block_index/0,
              block_offset/0,
              block_size/0,
              block_data/0,
              block/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("ahoy_block.hrl").

-type piece() :: pid().
-type piece_index() :: non_neg_integer().
-type piece_length() :: non_neg_integer().
-type raw_piece() :: binary().

-type block_index() :: non_neg_integer().
-type block_offset() :: non_neg_integer().
-type block_size() :: non_neg_integer().
-type block_data() :: binary() | atom().
-type block() :: {block_index(), block_data()}.

-record(state, {block_size :: block_size(),
                num_blocks :: non_neg_integer(),
                missing :: list(block()),
                pending :: list(block()),
                completed :: list(block()),
                last_block_size}).

%% @doc Create a new piece of specified piece length in bytes.
start_link(PieceLength) ->
    start_link(PieceLength, ?BLOCK_SIZE).

%% @doc Create a new piece of specified piece length and block size in bytes
start_link(PieceLength, BlockSize) ->
    start_link(PieceLength, BlockSize, false).

%% @doc Create the last piece, which is a special case.
start_link(PieceLength, BlockSize, LastPiece) ->
    gen_server:start_link(?MODULE, [PieceLength, BlockSize, LastPiece], []).

%% @doc Return tuple of the next missing block along with the expected block
%% size. False if there are no more missing blocks.
-spec pop_missing_block(piece()) -> {ok, {block(), block_size()}} | false.
pop_missing_block(Ref) ->
    gen_server:call(Ref, pop).

%% @doc Add missing block with the missing data as completed.
-spec add_completed_block(piece(), block()) -> ok.
add_completed_block(Ref, Block) ->
    gen_server:cast(Ref, {block, Block}).

%% @doc Return all blocks as a binary. False if not all blocks are completed.
-spec raw_piece(piece()) -> {ok, raw_piece()} | false.
raw_piece(Ref) ->
    gen_server:call(Ref, raw_piece).

init([PieceLength, BlockSize, false]) ->
    OneOrZero = case PieceLength rem BlockSize =:= 0 of
        true  -> 0;
        false -> 1
    end,
    NumBlocks = (PieceLength div BlockSize) + OneOrZero,
    Missing = [{X, empty} || X <- lists:seq(0, NumBlocks - 1)],
    Pending = [],
    Completed = [],
    State = #state{block_size = BlockSize, num_blocks = NumBlocks,
        missing = Missing, pending = Pending, completed = Completed,
        last_block_size = false},
    {ok, State};
init([_, BlockSize, {_, NumBlocks, LastBlockSize}]) ->
    Missing = [{X, empty} || X <- lists:seq(0, NumBlocks)],
    Pending = [],
    Completed = [],
    State = #state{block_size = BlockSize, num_blocks = NumBlocks,
        missing = Missing, pending = Pending, completed = Completed,
        last_block_size = LastBlockSize},
    {ok, State}.

handle_call(pop, _From, State=#state{missing=[]}) ->
    Reply = false,
    {reply, Reply, State};
handle_call(pop, _From, State=#state{block_size=BlockSize,
                                     last_block_size=LastBlockSize,
                                     missing=[Block|Missing],
                                     pending=Pending}) ->
    BlockSize2 = choose_block_size(BlockSize, LastBlockSize, Missing),
    Reply = {ok, {Block, BlockSize2}},
    Pending2 = [Block|Pending],
    State2 = State#state{missing=Missing, pending=Pending2},
    {reply, Reply, State2};
handle_call(raw_piece, _From, State=#state{missing=[],
                                           pending=[],
                                           completed=Completed}) ->

    Piece = block_to_binary(Completed),
    Reply = {ok, Piece},
    {reply, Reply, State};
handle_call(raw_piece, _From, State) ->
    {reply, false, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({block, {_, empty}}, State) ->
    {noreply, State};
handle_cast({block, {BlockIndex, BlockData}}, State=#state{block_size=BlockSize,
                                                           pending=Pending,
                                                           completed=Completed}) ->
    true = byte_size(BlockData) =< BlockSize,
    State2 = case lists:keytake(BlockIndex, 1, Pending) of
        {value, {BlockIndex, empty}, Pending2} ->
            Completed2 = [{BlockIndex, BlockData}|Completed],
            State#state{pending=Pending2, completed=Completed2};
        _ ->
            State
    end,
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Convert blocks to binary i.e. raw piece.
block_to_binary(Blocks) ->
    SortedBlocks = lists:keysort(1, Blocks),
    lists:foldl(fun block_fold/2, <<>>, SortedBlocks).

%% Function for folding blocks into binary.
block_fold({_, X}, Acc) ->
    <<Acc/binary, X/binary>>.

%% Return block size i.e. detect when this is the last block.
choose_block_size(BlockSize, false, _Missing) ->
    % is not last piece
    BlockSize;
choose_block_size(_BlockSize, LastBlockSize, []) ->
    % is last piece and last block
    LastBlockSize;
choose_block_size(BlockSize, _LastBlockSize, _Missing) ->
    % is last piece but not last block
    BlockSize.
