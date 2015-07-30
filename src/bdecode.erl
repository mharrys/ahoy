-module(bdecode).

-export([decode/1]).

%% Return decoded data from a file.
decode(Path) when is_list(Path) ->
    {ok, Bin} = file:read_file(Path),
    decode(Bin);
%% Return decoded data from binary data.
decode(Bin) when is_binary(Bin) ->
    {Value, _} = decode_type(Bin),
    Value.

decode_type(<<$i, T/binary>>) ->
    decode_int(T, []);
decode_type(<<$l, T/binary>>) ->
    decode_list(T, []);
decode_type(<<$d, T/binary>>) ->
    decode_dict(T, orddict:new());
decode_type(Bin) ->
    decode_string(Bin, []).

decode_int(<<$e, Rest/binary>>, Acc) ->
    Digits = lists:reverse(Acc),
    N = list_to_integer(Digits),
    {N, Rest};
decode_int(<<H, T/binary>>, Acc) ->
    decode_int(T, [H|Acc]).

decode_list(<<$e, T/binary>>, Acc) ->
    List = lists:reverse(Acc),
    {{list, List}, T};
decode_list(Bin, Acc) ->
    {Item, T} = decode_type(Bin),
    decode_list(T, [Item|Acc]).

decode_dict(<<$e, T/binary>>, Acc) ->
    {{dict, Acc}, T};
decode_dict(Bin, Acc) ->
    {Key, T} = decode_type(Bin),
    {Value, T2} = decode_type(T),
    decode_dict(T2, orddict:store(Key, Value, Acc)).

decode_string(<<$:, T/binary>>, Acc) ->
    Digits = lists:reverse(Acc),
    N = list_to_integer(Digits),
    <<Str:N/binary, T2/binary>> = T,
    {Str, T2};
decode_string(<<H, T/binary>>, Acc) ->
    decode_string(T, [H|Acc]).
