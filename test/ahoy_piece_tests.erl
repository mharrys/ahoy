-module(ahoy_piece_tests).

-include_lib("eunit/include/eunit.hrl").

starting_state_test() ->
    PieceLength = 1337,
    BlockSize = 1,
    {ok, Pid} = ahoy_piece:start_link(PieceLength, false, BlockSize),
    % expect specified number of missing blocks
    Missing = [ahoy_piece:pop_missing_block(Pid) || _ <- lists:seq(0, PieceLength - 1)],
    ?assertEqual(false, lists:member(false, Missing)),
    ?assertEqual(false, ahoy_piece:pop_missing_block(Pid)).

incomplete_piece_test() ->
    PieceLength = 4,
    BlockSize = 1,
    BlockData = <<1:BlockSize>>,
    {ok, Pid} = ahoy_piece:start_link(PieceLength, false, BlockSize),
    ?assertEqual(false, ahoy_piece:raw_piece(Pid)),
    {ok, {{I1, empty}, BlockSize}} = ahoy_piece:pop_missing_block(Pid),
    {ok, {{I2, empty}, BlockSize}} = ahoy_piece:pop_missing_block(Pid),
    ahoy_piece:add_completed_block(Pid, {I1, BlockData}),
    ahoy_piece:add_completed_block(Pid, {I2, BlockData}),
    ahoy_piece:add_completed_block(Pid, {I2 + 1, BlockData}),
    ahoy_piece:add_completed_block(Pid, {I2 + 2, BlockData}),
    ?assertEqual(false, ahoy_piece:raw_piece(Pid)).

completed_piece_test() ->
    PieceLength = 8,
    BlockSize = 4,
    NumBits = BlockSize * 8,
    Data1 = <<1:NumBits>>,
    Data2 = <<2:NumBits>>,
    {ok, Pid} = ahoy_piece:start_link(PieceLength, false, BlockSize),
    ?assertEqual(false, ahoy_piece:raw_piece(Pid)),
    {ok, {{I1, empty}, BlockSize}} = ahoy_piece:pop_missing_block(Pid),
    {ok, {{I2, empty}, BlockSize}} = ahoy_piece:pop_missing_block(Pid),
    % add in another order than requested
    ahoy_piece:add_completed_block(Pid, {I2, Data2}),
    ahoy_piece:add_completed_block(Pid, {I1, Data1}),
    % expect sorted data
    Piece = <<Data1/binary, Data2/binary>>,
    ?assertEqual({ok, Piece}, ahoy_piece:raw_piece(Pid)).
