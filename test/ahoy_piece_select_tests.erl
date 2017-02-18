-module(ahoy_piece_select_tests).

-include_lib("eunit/include/eunit.hrl").

starting_state_test() ->
    PieceCount = 1234,
    {ok, Pid} = ahoy_bitfield:start_link(PieceCount),
    {ok, Pid2} = ahoy_piece_stat:start_link(PieceCount),
    {ok, Pid3} = ahoy_piece_select:start_link(Pid, Pid2),
    Expected = [],
    Actual = ahoy_piece_select:reserve(Pid3, PieceCount),
    ?assertEqual(Expected, Actual).

reserve_test() ->
    PieceCount = 1355,
    {ok, Pid} = ahoy_bitfield:start_link(PieceCount),
    {ok, Pid2} = ahoy_piece_stat:start_link(PieceCount),
    {ok, Pid3} = ahoy_piece_select:start_link(Pid, Pid2),
    Have = [20, 39, 100, 1223, 0],
    [ahoy_piece_stat:new_piece(Pid2, X) || X <- Have],
    Expected = lists:sort(Have),
    Actual = ahoy_piece_select:reserve(Pid3, length(Have)),
    ?assertEqual(Expected, Actual),
    % pieces should not be picked again once reserved
    Expected2 = [],
    Actual2 = ahoy_piece_select:reserve(Pid3, length(Have)),
    ?assertEqual(Expected2, Actual2),
    % reserve new
    % when reserving new, make sure the previous reserved are still reserved
    Have2 = [50, 20, 29],
    [ahoy_piece_stat:new_piece(Pid2, X) || X <- Have2],
    Expected3 = [29, 50], % 20 is already reserved
    Actual3 = ahoy_piece_select:reserve(Pid3, length(Have2)),
    ?assertEqual(Expected3, Actual3).

reserve_bitfield_test() ->
    PieceCount = 932,
    {ok, Pid} = ahoy_bitfield:start_link(PieceCount),
    {ok, Pid2} = ahoy_piece_stat:start_link(PieceCount),
    {ok, Pid3} = ahoy_piece_select:start_link(Pid, Pid2),
    Have = [1, 883, 10, 13, 8],
    [ahoy_piece_stat:new_piece(Pid2, X) || X <- Have],
    Downloaded = [1, 10, 13],
    [ahoy_bitfield:set(Pid, X) || X <- Downloaded],
    % should not return what we set in our bitfield
    Expected = [8, 883],
    Actual = ahoy_piece_select:reserve(Pid3, length(Have)),
    ?assertEqual(Expected, Actual).

unreserve_test() ->
    PieceCount = 2391,
    {ok, Pid} = ahoy_bitfield:start_link(PieceCount),
    {ok, Pid2} = ahoy_piece_stat:start_link(PieceCount),
    {ok, Pid3} = ahoy_piece_select:start_link(Pid, Pid2),
    Have = [399, 9, 0, 1, 2300],
    [ahoy_piece_stat:new_piece(Pid2, X) || X <- Have],
    % we get the pieces we have at least one of
    Expected = lists:sort(Have),
    Actual = ahoy_piece_select:reserve(Pid3, length(Have)),
    ?assertEqual(Expected, Actual),
    % pieces are now reserved
    Expected2 = [],
    Actual2 = ahoy_piece_select:reserve(Pid3, length(Have)),
    ?assertEqual(Expected2, Actual2),
    % unreserve pieces
    ahoy_piece_select:unreserve(Pid3, Have),
    Expected3 = lists:sort(Have),
    Actual3 = ahoy_piece_select:reserve(Pid3, length(Have)),
    ?assertEqual(Expected3, Actual3).
