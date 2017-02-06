-module(ahoy_percent_encoding).

-export([encode/1]).

%% @doc Percent-encode data. See RFC 3986 for details.
-spec encode(binary() | list(integer())) -> string().
encode(Bin) when is_binary(Bin) ->
    encode(binary_to_list(Bin));
encode(List) when is_list(List) ->
    lists:flatmap(fun encode_char/1, List).

-spec encode_char(integer()) -> list(integer()).
encode_char(C) ->
    case is_unreserved(C) of
        true  -> [C];
        false -> [$% | two_digit_hex(C)]
    end.

-spec is_unreserved(integer()) -> boolean().
is_unreserved(C) ->
    ((C >= $a) and (C =< $z)) or
    ((C >= $A) and (C =< $Z)) or
    ((C >= $0) and (C =< $9)) or
    lists:member(C, [$-, $., $_, $~]).

-spec two_digit_hex(integer()) -> string().
two_digit_hex(C) when C < 256 ->
    [hex(C div 16), hex(C rem 16)].

-spec hex(integer()) -> integer().
hex(C) when C < 10 ->
    $0 + C;
hex(C) when C >= 10, C < 16 ->
    $a + (C - 10).
