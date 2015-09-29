-module(skur_bencode_tests).

-include_lib("eunit/include/eunit.hrl").

encode_int_test() ->
    ?assertEqual(<<"i42e">>, skur_bencode:encode(42)),
    ?assertEqual(<<"i-42e">>, skur_bencode:encode(-42)),
    ?assertEqual(<<"i0e">>, skur_bencode:encode(0)),
    ?assertEqual(<<"i9223372036854775808e">>,
                   skur_bencode:encode(9223372036854775808)).

encode_string_test() ->
    ?assertEqual(<<"0:">>, skur_bencode:encode(<<"">>)),
    ?assertEqual(<<"6:foobar">>, skur_bencode:encode(<<"foobar">>)),
    ?assertEqual(<<"12:foobarfoobar">>, skur_bencode:encode(<<"foobarfoobar">>)).

encode_list_test() ->
    ?assertEqual(<<"le">>, skur_bencode:encode({list, []})),
    ?assertEqual(<<"llleee">>, skur_bencode:encode({list, [{list, [{list, []}]}]})),
    ?assertEqual(<<"li42ee">>, skur_bencode:encode({list, [42]})),
    ?assertEqual(<<"l4:spame">>, skur_bencode:encode({list, [<<"spam">>]})),
    ?assertEqual(<<"li42e4:spame">>, skur_bencode:encode({list, [42, <<"spam">>]})),
    ?assertEqual(<<"lli42eel4:spamee">>, skur_bencode:encode({list, [{list, [42]}, {list, [<<"spam">>]}]})).

encode_dict_test() ->
    ?assertEqual(<<"de">>, skur_bencode:encode({dict, orddict:new()})),
    D1 = orddict:from_list([{<<"foo">>, 42}]),
    ?assertEqual(<<"d3:fooi42ee">>, skur_bencode:encode({dict, D1})),
    D2 = orddict:from_list([{<<"foobarfoobar">>, 10}]),
    D3 = orddict:store(<<"nested">>, {dict, D2}, D1),
    ?assertEqual(<<"d3:fooi42e6:nestedd12:foobarfoobari10eee">>, skur_bencode:encode({dict, D3})).
