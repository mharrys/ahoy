-module(ahoy_bdecode_tests).

-include_lib("eunit/include/eunit.hrl").

decode_int_test() ->
    ?assertEqual(42, ahoy_bdecode:decode(<<"i42e">>)),
    ?assertEqual(-42, ahoy_bdecode:decode(<<"i-42e">>)),
    ?assertEqual(0, ahoy_bdecode:decode(<<"i0e">>)),
    ?assertEqual(9223372036854775808, ahoy_bdecode:decode(<<"i9223372036854775808e">>)),
    ?_assertException(error, function_clause, ahoy_bdecode:decode(<<"i">>)).

decode_string_test() ->
    ?assertEqual(<<"">>, ahoy_bdecode:decode(<<"0:">>)),
    ?assertEqual(<<"foobar">>, ahoy_bdecode:decode(<<"6:foobar">>)),
    ?assertEqual(<<"foobarfoobar">>, ahoy_bdecode:decode(<<"12:foobarfoobar">>)),
    ?assertEqual(<<"b">>, ahoy_bdecode:decode(<<"1:baz">>)),
    ?_assertException(error, function_clause, ahoy_bdecode:decode(<<"4:baz">>)).

decode_list_test() ->
    ?assertEqual({list, []}, ahoy_bdecode:decode(<<"le">>)),
    ?assertEqual({list, [{list, [{list, []}]}]}, ahoy_bdecode:decode(<<"llleee">>)),
    ?assertEqual({list, [42]}, ahoy_bdecode:decode(<<"li42ee">>)),
    ?assertEqual({list, [<<"spam">>]}, ahoy_bdecode:decode(<<"l4:spame">>)),
    ?assertEqual({list, [42, <<"spam">>]}, ahoy_bdecode:decode(<<"li42e4:spame">>)),
    ?assertEqual({list, [{list, [42]}, {list, [<<"spam">>]}]}, ahoy_bdecode:decode(<<"lli42eel4:spamee">>)),
    ?_assertException(error, function_clause, ahoy_bdecode:decode(<<"l">>)).

decode_dict_test() ->
    ?assertEqual({dict, orddict:new()}, ahoy_bdecode:decode(<<"de">>)),
    {dict, D1} = ahoy_bdecode:decode(<<"d3:fooi42ee">>),
    ?assertEqual({ok, 42}, orddict:find(<<"foo">>, D1)),
    {dict, D2} = ahoy_bdecode:decode(<<"d3:food12:foobarfoobari42eee">>),
    {ok, {dict, D3}} = orddict:find(<<"foo">>, D2),
    ?assertEqual({ok, 42}, orddict:find(<<"foobarfoobar">>, D3)).
