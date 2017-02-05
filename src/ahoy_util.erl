-module(ahoy_util).

-export([encode_url/1]).


%% URL encodes binary data according to RFC1738 only needed because
%% http_uri:encode can't handle binary
encode_url(Bytes) ->
    encode_url(Bytes, "").
encode_url(<<>>, Acc) ->
    Acc;
encode_url(<<Byte:8, Rest/binary>>, Acc) when Byte < 16 ->
    encode_url(Rest, Acc ++ [$%|[$0|integer_to_list(Byte, 16)]]);
encode_url(<<Byte:8, Rest/binary>>, Acc) when Byte < 32; Byte >= 126; Byte == 92 ->
    encode_url(Rest, Acc ++ [$%|integer_to_list(Byte, 16)]);
encode_url(<<Byte:8, Rest/binary>>, Acc) ->
    encode_url(Rest, Acc ++ [Byte]).
