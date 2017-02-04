-module(ahoy_bencode_tests).

-include_lib("eunit/include/eunit.hrl").

encode_int_test() ->
    ?assertEqual(<<"i42e">>, ahoy_bencode:encode(42)),
    ?assertEqual(<<"i-42e">>, ahoy_bencode:encode(-42)),
    ?assertEqual(<<"i0e">>, ahoy_bencode:encode(0)),
    ?assertEqual(<<"i9223372036854775808e">>, ahoy_bencode:encode(9223372036854775808)).

encode_string_test() ->
    ?assertEqual(<<"0:">>, ahoy_bencode:encode(<<"">>)),
    ?assertEqual(<<"6:foobar">>, ahoy_bencode:encode(<<"foobar">>)),
    ?assertEqual(<<"12:foobarfoobar">>, ahoy_bencode:encode(<<"foobarfoobar">>)).

encode_list_test() ->
    ?assertEqual(<<"le">>, ahoy_bencode:encode({list, []})),
    ?assertEqual(<<"llleee">>, ahoy_bencode:encode({list, [{list, [{list, []}]}]})),
    ?assertEqual(<<"li42ee">>, ahoy_bencode:encode({list, [42]})),
    ?assertEqual(<<"l4:spame">>, ahoy_bencode:encode({list, [<<"spam">>]})),
    ?assertEqual(<<"li42e4:spame">>, ahoy_bencode:encode({list, [42, <<"spam">>]})),
    ?assertEqual(<<"lli42eel4:spamee">>, ahoy_bencode:encode({list, [{list, [42]}, {list, [<<"spam">>]}]})).

encode_dict_test() ->
    ?assertEqual(<<"de">>, ahoy_bencode:encode({dict, orddict:new()})),
    D1 = orddict:from_list([{<<"foo">>, 42}]),
    ?assertEqual(<<"d3:fooi42ee">>, ahoy_bencode:encode({dict, D1})),
    D2 = orddict:from_list([{<<"foobarfoobar">>, 10}]),
    D3 = orddict:store(<<"nested">>, {dict, D2}, D1),
    ?assertEqual(<<"d3:fooi42e6:nestedd12:foobarfoobari10eee">>, ahoy_bencode:encode({dict, D3})).
