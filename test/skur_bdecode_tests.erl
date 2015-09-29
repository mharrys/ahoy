-module(skur_bdecode_tests).

-include_lib("eunit/include/eunit.hrl").

decode_int_test() ->
    ?assertEqual(42, skur_bdecode:decode(<<"i42e">>)),
    ?assertEqual(-42, skur_bdecode:decode(<<"i-42e">>)),
    ?assertEqual(0, skur_bdecode:decode(<<"i0e">>)),
    ?assertEqual(9223372036854775808,
                 skur_bdecode:decode(<<"i9223372036854775808e">>)),
    ?_assertException(error, function_clause, skur_bdecode:decode(<<"i">>)).

decode_string_test() ->
    ?assertEqual(<<"">>, skur_bdecode:decode(<<"0:">>)),
    ?assertEqual(<<"foobar">>, skur_bdecode:decode(<<"6:foobar">>)),
    ?assertEqual(<<"foobarfoobar">>, skur_bdecode:decode(<<"12:foobarfoobar">>)),
    ?assertEqual(<<"b">>, skur_bdecode:decode(<<"1:baz">>)),
    ?_assertException(error, function_clause, skur_bdecode:decode(<<"4:baz">>)).

decode_list_test() ->
    ?assertEqual({list, []}, skur_bdecode:decode(<<"le">>)),
    ?assertEqual({list, [{list, [{list, []}]}]}, skur_bdecode:decode(<<"llleee">>)),
    ?assertEqual({list, [42]}, skur_bdecode:decode(<<"li42ee">>)),
    ?assertEqual({list, [<<"spam">>]}, skur_bdecode:decode(<<"l4:spame">>)),
    ?assertEqual({list, [42, <<"spam">>]}, skur_bdecode:decode(<<"li42e4:spame">>)),
    ?assertEqual({list, [{list, [42]}, {list, [<<"spam">>]}]}, skur_bdecode:decode(<<"lli42eel4:spamee">>)),
    ?_assertException(error, function_clause, skur_bdecode:decode(<<"l">>)).

decode_dict_test() ->
    ?assertEqual({dict, orddict:new()}, skur_bdecode:decode(<<"de">>)),
    {dict, D1} = skur_bdecode:decode(<<"d3:fooi42ee">>),
    ?assertEqual({ok, 42}, orddict:find(<<"foo">>, D1)),
    {dict, D2} = skur_bdecode:decode(<<"d3:food12:foobarfoobari42eee">>),
    {ok, {dict, D3}} = orddict:find(<<"foo">>, D2),
    ?assertEqual({ok, 42}, orddict:find(<<"foobarfoobar">>, D3)).
