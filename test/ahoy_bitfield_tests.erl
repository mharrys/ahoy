-module(ahoy_bitfield_tests).

-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    {ok, Pid} = ahoy_bitfield:start_link(0),
    ?assertEqual(<<>>, ahoy_bitfield:raw_bitfield(Pid)),
    is_bitfield_empty(Pid),
    is_bitfield_binary(Pid).

starting_state_test() ->
    N = 2000,
    {ok, Pid} = ahoy_bitfield:start_link(N),
    Xs = [ahoy_bitfield:is_set(Pid, X) || X <- lists:seq(0, N - 1)],
    Ys = lists:map(fun(_) -> false end, lists:seq(0, N - 1)),
    ?assertEqual(Ys, Xs),
    is_bitfield_empty(Pid),
    is_bitfield_binary(Pid).

set_all_test() ->
    N = 2000,
    {ok, Pid} = ahoy_bitfield:start_link(N),
    [ahoy_bitfield:set(Pid, X) || X <- lists:seq(0, N - 1)],
    Xs = [ahoy_bitfield:is_set(Pid, X) || X <- lists:seq(0, N - 1)],
    Ys = lists:map(fun(_) -> true end, lists:seq(0, N - 1)),
    ?assertEqual(Ys, Xs),
    is_bitfield_non_empty(Pid),
    is_bitfield_binary(Pid).

random_sets_test() ->
    N = 2000,
    Is = [102, 0, 150, 23, 100, 500, 1093, 22, 102, 102, 1950, 1999],
    {ok, Pid} = ahoy_bitfield:start_link(N),
    [ahoy_bitfield:set(Pid, X) || X <- Is],
    Xs = [ahoy_bitfield:is_set(Pid, X) || X <- lists:seq(0, N - 1)],
    Ys = lists:map(fun(I) -> lists:member(I, Is) end, lists:seq(0, N - 1)),
    ?assertEqual(Ys, Xs),
    is_bitfield_non_empty(Pid),
    is_bitfield_binary(Pid).

is_bitfield_empty(Pid) ->
    ?assertEqual(true, ahoy_bitfield:is_empty(Pid)).

is_bitfield_non_empty(Pid) ->
    ?assertEqual(false, ahoy_bitfield:is_empty(Pid)).

is_bitfield_binary(Pid) ->
    ?assertEqual(true, is_binary(ahoy_bitfield:raw_bitfield(Pid))).
