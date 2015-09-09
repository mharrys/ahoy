-module(bdecode_tests).

-include_lib("eunit/include/eunit.hrl").

decode_int_test() ->
    ?assertEqual(42, bdecode:decode(<<"i42e">>)),
    ?assertEqual(-42, bdecode:decode(<<"i-42e">>)),
    ?assertEqual(0, bdecode:decode(<<"i0e">>)),
    ?assertEqual(9223372036854775808,
                 bdecode:decode(<<"i9223372036854775808e">>)),
    ?_assertException(error, function_clause, bdecode:decode(<<"i">>)).

decode_string_test() ->
    ?assertEqual(<<"">>, bdecode:decode(<<"0:">>)),
    ?assertEqual(<<"foobar">>, bdecode:decode(<<"6:foobar">>)),
    ?assertEqual(<<"foobarfoobar">>, bdecode:decode(<<"12:foobarfoobar">>)),
    ?assertEqual(<<"b">>, bdecode:decode(<<"1:baz">>)),
    ?_assertException(error, function_clause, bdecode:decode(<<"4:baz">>)).

decode_list_test() ->
    ?assertEqual({list, []}, bdecode:decode(<<"le">>)),
    ?assertEqual({list, [{list, [{list, []}]}]}, bdecode:decode(<<"llleee">>)),
    ?assertEqual({list, [42]}, bdecode:decode(<<"li42ee">>)),
    ?assertEqual({list, [<<"spam">>]}, bdecode:decode(<<"l4:spame">>)),
    ?assertEqual({list, [42, <<"spam">>]}, bdecode:decode(<<"li42e4:spame">>)),
    ?assertEqual({list, [{list, [42]}, {list, [<<"spam">>]}]}, bdecode:decode(<<"lli42eel4:spamee">>)),
    ?_assertException(error, function_clause, bdecode:decode(<<"l">>)).

decode_dict_test() ->
    ?assertEqual({dict, orddict:new()}, bdecode:decode(<<"de">>)),
    {dict, D1} = bdecode:decode(<<"d3:fooi42ee">>),
    ?assertEqual({ok, 42}, orddict:find(<<"foo">>, D1)),
    {dict, D2} = bdecode:decode(<<"d3:food12:foobarfoobari42eee">>),
    {ok, {dict, D3}} = orddict:find(<<"foo">>, D2),
    ?assertEqual({ok, 42}, orddict:find(<<"foobarfoobar">>, D3)).

raw_test() ->
    D = <<"d3:food12:foobarfoobari42eee">>,
    ?assertEqual({ok, <<"d12:foobarfoobari42ee">>}, bdecode:raw(D, <<"foo">>)).