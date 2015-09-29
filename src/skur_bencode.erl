-module(skur_bencode).

-export([encode/1]).

encode(Data) ->
    encode_type(Data).

encode_type(Data) when is_integer(Data) ->
    N = list_to_binary(integer_to_list(Data)),
    <<$i, N/binary, $e>>;
encode_type(Data) when is_binary(Data) ->
    N = list_to_binary(integer_to_list(size(Data))),
    <<N/binary, $:, Data/binary>>;
encode_type({list, Data}) ->
    Content = list_to_binary([encode_type(X) || X <- Data]),
    <<$l, Content/binary, $e>>;
encode_type({dict, Data}) ->
    Content = orddict:fold(fun encode_dict_item/3, <<>>, Data),
    <<$d, Content/binary, $e>>.

encode_dict_item(Key, Value, Acc) ->
    KeyBin = encode_type(Key),
    ValueBin = encode_type(Value),
    <<Acc/binary, KeyBin/binary, ValueBin/binary>>.
