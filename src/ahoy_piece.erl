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

-export([factory/3,
         start_link/1,
         start_link/2,
         pop_missing_block/1,
         add_completed_block/2,
         raw_piece/1]).

-export_type([piece/0,
              piece_index/0,
              piece_length/0,
              raw_piece/0,
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

-define(BLOCK_SIZE, 16384).

-type piece() :: pid().
-type piece_index() :: non_neg_integer().
-type piece_length() :: non_neg_integer().
-type raw_piece() :: binary().

-type block_offset() :: non_neg_integer().
-type block_size() :: non_neg_integer().
-type block_data() :: binary() | atom().
-type block() :: {block_offset(), block_size(), block_data()}.

-record(state, {missing :: list(block()),
                pending :: list(block()),
                completed :: list(block())}).

%% @doc Factory function for creating a piece.
factory(TorrentLength, PieceCount, PieceLength) ->
    Blocks = gen_blocks(PieceLength, ?BLOCK_SIZE),
    fun (PieceIndex) ->
        Blocks2 = case PieceIndex =:= (PieceCount - 1) of
            true ->
                gen_blocks_last(TorrentLength, PieceLength, ?BLOCK_SIZE);
            false ->
                Blocks
        end,
        start_link(Blocks2)
    end.

%% @doc Create a new piece from blocks.
start_link(Blocks) ->
    gen_server:start_link(?MODULE, [Blocks], []).

%% @doc Create a new piece for a piece of specified length and with specified
%% block size.
start_link(PieceLength, BlockSize) ->
    Blocks = gen_blocks(PieceLength, BlockSize),
    start_link(Blocks).

%% @doc Return tuple of the next missing block along with the expected block
%% size. False if there are no more missing blocks.
-spec pop_missing_block(piece()) -> {ok, block()} | false.
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

init([Blocks]) ->
    State = #state{missing=Blocks, pending=[], completed=[]},
    {ok, State}.

handle_call(pop, _From, State=#state{missing=[]}) ->
    Reply = false,
    {reply, Reply, State};
handle_call(pop, _From, State=#state{missing=Missing, pending=Pending}) ->
    [Block|Missing2] = Missing,
    Reply = {ok, Block},
    Pending2 = [Block|Pending],
    State2 = State#state{missing=Missing2, pending=Pending2},
    {reply, Reply, State2};
handle_call(raw_piece, _From, State=#state{missing=[], pending=[], completed=Completed}) ->
    Piece = block_to_binary(Completed),
    Reply = {ok, Piece},
    {reply, Reply, State};
handle_call(raw_piece, _From, State) ->
    {reply, false, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({block, {_, _, empty}}, State) ->
    {noreply, State};
handle_cast({block, Block}, State=#state{pending=Pending, completed=Completed}) ->
    {BlockOffset, BlockSize, _BlockData} = Block,
    State2 = case lists:keytake(BlockOffset, 1, Pending) of
        {value, {BlockOffset, BlockSize, empty}, Pending2} ->
            Completed2 = [Block|Completed],
            State#state{pending=Pending2, completed=Completed2};
        _ ->
            State
    end,
    {noreply, State2};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Generate one block.
gen_block(BlockIndex, BlockSize) ->
    gen_block(BlockIndex, BlockSize, BlockSize).

gen_block(BlockIndex, BlockSize, BlockOffsetSize) ->
    {BlockIndex * BlockOffsetSize, BlockSize, empty}.

%% Generate blocks of specified size that is required to fill specified piece length.
gen_blocks(PieceLength, BlockSize) ->
    NumBlocks = num_blocks(PieceLength, BlockSize),
    [gen_block(X, BlockSize) || X <- lists:seq(0, NumBlocks - 1)].

%% Same as gen_blocks but for last piece.
gen_blocks_last(TorrentLength, PieceLength, BlockSize) ->
    LastPieceSize = TorrentLength rem PieceLength,
    % full blocks
    NumFullBlocks = LastPieceSize div BlockSize,
    FullBlocks = [gen_block(X, BlockSize) || X <- lists:seq(0, NumFullBlocks - 1)],
    % last block
    LastBlockIndex = NumFullBlocks,
    LastBlockSize = LastPieceSize - (NumFullBlocks * BlockSize),
    LastBlock = gen_block(LastBlockIndex, LastBlockSize, BlockSize),
    % check if there is a block of smaller size needed at the end of the piece
    case LastBlockSize > 0 of
        true  -> FullBlocks ++ [LastBlock];
        false -> FullBlocks
    end.

%% Return minimum number of blocks of specified size to fill a piece of specified length.
num_blocks(PieceLength, BlockSize) ->
    OneOrZero = case PieceLength rem BlockSize =:= 0 of
        true  -> 0;
        false -> 1
    end,
    (PieceLength div BlockSize) + OneOrZero.

%% Convert blocks to binary i.e. raw piece.
block_to_binary(Blocks) ->
    SortedBlocks = lists:keysort(1, Blocks),
    lists:foldl(fun block_fold/2, <<>>, SortedBlocks).

%% Function for folding blocks into binary.
block_fold({_, _, X}, Acc) ->
    <<Acc/binary, X/binary>>.
