-module(ahoy_piece_stat_tests).

-include_lib("eunit/include/eunit.hrl").

starting_state_test() ->
    PieceCount = 16,
    {ok, Pid} = ahoy_piece_stat:start_link(PieceCount),
    Expected = [{X, 0} || X <- lists:seq(0, PieceCount - 1)],
    Sorted = ahoy_piece_stat:sorted(Pid),
    ?assertEqual(Expected, Sorted).

new_bitfield_test() ->
    PieceCount = 16,
    {ok, Pid} = ahoy_piece_stat:start_link(PieceCount),
    {ok, Pid2} = ahoy_bitfield:start_link(PieceCount),
    Have = [1, 15, 8, 3, 0],
    [ahoy_bitfield:set(Pid2, X) || X <- Have],
    Bitfield = ahoy_bitfield:raw_bitfield(Pid2),
    ahoy_piece_stat:add_bitfield(Pid, Bitfield),
    Expected = expected_list(PieceCount, Have),
    Sorted = ahoy_piece_stat:sorted(Pid),
    ?assertEqual(Expected, Sorted),
    % one more count
    ahoy_piece_stat:add_bitfield(Pid, Bitfield),
    Expected2 = lists:map(fun inc_non_zero/1, Expected),
    Sorted2 = ahoy_piece_stat:sorted(Pid),
    ?assertEqual(Expected2, Sorted2).

new_piece_test() ->
    PieceCount = 16,
    {ok, Pid} = ahoy_piece_stat:start_link(PieceCount),
    Have = [0, 3, 5, 8, 15],
    [ahoy_piece_stat:add_piece_index(Pid, X) || X <- Have],
    % expecting all elements that are not in have to be first with 0 count,
    % and everything in have to be last with 1 count
    Expected = expected_list(PieceCount, Have),
    Sorted = ahoy_piece_stat:sorted(Pid),
    ?assertEqual(Expected, Sorted),
    % double count
    [ahoy_piece_stat:add_piece_index(Pid, X) || X <- Have],
    Expected2 = lists:map(fun inc_non_zero/1, Expected),
    Sorted2 = ahoy_piece_stat:sorted(Pid),
    ?assertEqual(Expected2, Sorted2),
    % one extra with 1 count
    Special = 9,
    ahoy_piece_stat:add_piece_index(Pid, Special),
    {L1, L2} = lists:split(PieceCount - length(Have), Expected2),
    L3 = lists:keydelete(Special, 1, L1),
    Expected3 = L3 ++ [{Special, 1}] ++ L2,
    Sorted3 = ahoy_piece_stat:sorted(Pid),
    ?assertEqual(Expected3, Sorted3).

expected_list(PieceCount, Have) ->
    L = [{X, 0} || X <- lists:seq(0, PieceCount - 1)],
    L2 = lists:filter(fun({X, _}) -> not(lists:member(X, Have)) end, L),
    L3 = lists:subtract(L, L2),
    L4 = lists:map(fun({X, _}) -> {X, 1} end, L3),
    L2 ++ L4.

inc_non_zero(Elem={_Index, 0}) ->
    Elem;
inc_non_zero({Index, Count}) ->
    {Index, Count + 1}.
