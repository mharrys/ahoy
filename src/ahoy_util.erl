-module(ahoy_util).

-export([request_url/2,
         encode_url/1]).

%% Creates a URL from a set of parameters so that {key, val} becomes &key=val
%% with URL encoding
request_url(Url, Parameters) ->
    request_url(Url, Parameters, "").
request_url(Url, [], Acc) ->
    Url ++ "?" ++ Acc;
request_url(Url, [{Key, Value}|Parameters], Acc) when is_integer(Value) ->
    NewAcc = Acc ++ http_uri:encode(Key) ++ "=" ++ integer_to_list(Value) ++ "&",
    request_url(Url, Parameters, NewAcc);
request_url(Url, [{Key, Value}|Parameters], Acc) when is_binary(Value) ->
    NewAcc = Acc ++ http_uri:encode(Key) ++ "=" ++ encode_url(Value) ++ "&",
    request_url(Url, Parameters, NewAcc);
request_url(Url, [{Key, Value}|Parameters], Acc) ->
    NewAcc = Acc ++ http_uri:encode(Key) ++ "=" ++ http_uri:encode(Value) ++ "&",
    request_url(Url, Parameters, NewAcc).

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
