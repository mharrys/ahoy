-module(bencode_tests).

-include_lib("eunit/include/eunit.hrl").

encode_int_test() ->
    ?assertEqual(<<"i42e">>, bencode:encode(42)),
    ?assertEqual(<<"i-42e">>, bencode:encode(-42)),
    ?assertEqual(<<"i0e">>, bencode:encode(0)),
    ?assertEqual(<<"i9223372036854775808e">>,
                 bencode:encode(9223372036854775808)).

encode_string_test() ->
    ?assertEqual(<<"0:">>, bencode:encode(<<"">>)),
    ?assertEqual(<<"6:foobar">>, bencode:encode(<<"foobar">>)),
    ?assertEqual(<<"12:foobarfoobar">>, bencode:encode(<<"foobarfoobar">>)).

encode_list_test() ->
    ?assertEqual(<<"le">>, bencode:encode({list, []})),
    ?assertEqual(<<"llleee">>, bencode:encode({list, [{list, [{list, []}]}]})),
    ?assertEqual(<<"li42ee">>, bencode:encode({list, [42]})),
    ?assertEqual(<<"l4:spame">>, bencode:encode({list, [<<"spam">>]})),
    ?assertEqual(<<"li42e4:spame">>, bencode:encode({list, [42, <<"spam">>]})),
    ?assertEqual(<<"lli42eel4:spamee">>, bencode:encode({list, [{list, [42]}, {list, [<<"spam">>]}]})).

encode_dict_test() ->
    ?assertEqual(<<"de">>, bencode:encode({dict, orddict:new()})),
    D1 = orddict:from_list([{<<"foo">>, 42}]),
    ?assertEqual(<<"d3:fooi42ee">>, bencode:encode({dict, D1})),
    D2 = orddict:from_list([{<<"foobarfoobar">>, 10}]),
    D3 = orddict:store(<<"nested">>, {dict, D2}, D1),
    ?assertEqual(<<"d3:fooi42e6:nestedd12:foobarfoobari10eee">>, bencode:encode({dict, D3})).
