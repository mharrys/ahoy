-module(ahoy_percent_encoding_tests).

-include_lib("eunit/include/eunit.hrl").

encode_empty_test() ->
    ?assertEqual("", ahoy_percent_encoding:encode([])).

encode_alpha_test() ->
    Lo = [X || X <- lists:seq($a, $z)],
    Hi = [X || X <- lists:seq($A, $Z)],
    Alpha = Lo ++ Hi,
    ?assertEqual(Alpha, ahoy_percent_encoding:encode(Alpha)).

encode_digit_test() ->
    Digit = [X || X <- lists:seq($0, $9)],
    ?assertEqual(Digit, ahoy_percent_encoding:encode(Digit)).

encode_unreserved_test() ->
    Unreserved = [$-, $., $_, $~],
    ?assertEqual(Unreserved, ahoy_percent_encoding:encode(Unreserved)).

encode_with_zero_padding_test() ->
    ?assertEqual("%00", ahoy_percent_encoding:encode([0])),
    ?assertEqual("%09", ahoy_percent_encoding:encode([9])),
    ?assertEqual("%0f", ahoy_percent_encoding:encode([15])).

encode_info_hash_test() ->
    InfoHash1 = ahoy_percent_encoding:encode(<<175, 84, 135, 186, 53, 198, 66,
        114, 49, 131, 226, 201, 62, 169, 106, 148, 251, 116, 43, 157>>),
    InfoHash2 = ahoy_percent_encoding:encode(<<201, 212, 213, 136, 77, 28, 221,
        211, 116, 95, 170, 140, 234, 182,  188, 52, 130, 150, 61, 144>>),
    ?assertEqual("%afT%87%ba5%c6Br1%83%e2%c9%3e%a9j%94%fbt%2b%9d", InfoHash1),
    ?assertEqual("%c9%d4%d5%88M%1c%dd%d3t_%aa%8c%ea%b6%bc4%82%96%3d%90", InfoHash2).
