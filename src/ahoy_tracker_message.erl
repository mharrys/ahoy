-module(ahoy_tracker_message).

-export([encode_request/3,
         decode_response/1]).

-include_lib("ahoy_metainfo.hrl").
-include_lib("ahoy_tracker_progress.hrl").
-include_lib("ahoy_tracker_response.hrl").

%% @doc Encode HTTP GET request URL of active peers to a tracker.
-spec encode_request(metainfo(), port_number(), tracker_progress()) -> string().
encode_request(Meta, Port, Progress) ->
    #metainfo{announce=Announce, info_hash=InfoHash} = Meta,
    #tracker_progress{uploaded=Up, downloaded=Down, left=Left} = Progress,
    Fmt = "~s?info_hash=~s&peer_id=~s&port=~b&&uploaded=~b&downloaded=~b&left=~b&compact=1",
    Args = [
        Announce,
        ahoy_percent_encoding:encode(InfoHash),
        ?PEER_ID,
        Port,
        Up,
        Down,
        Left
    ],
    io_lib:format(Fmt, Args).

%% @doc Decode tracker response body as a defined record.
-spec decode_response(list(string())) -> tracker_response().
decode_response(Body) ->
    {dict, Resp} = ahoy_bdecode:decode(list_to_binary(Body)),
    {_, Complete} = lists:keyfind(<<"complete">>, 1, Resp),
    {_, Incomplete} = lists:keyfind(<<"incomplete">>, 1, Resp),
    {_, Interval} = lists:keyfind(<<"interval">>, 1, Resp),
    {_, RawPeers} = lists:keyfind(<<"peers">>, 1, Resp),
    Peers = read_peers(RawPeers),
    #tracker_response{
        complete = Complete,
        incomplete = Incomplete,
        interval = Interval,
        peers = Peers
    }.

%% @doc Re-interpret byte stream as list of peer records.
-spec read_peers(binary()) -> list(peer()).
read_peers(Bytes) ->
    read_peers(Bytes, []).
read_peers(<<>>, Acc) ->
    Acc;
read_peers(<<IP1:8, IP2:8, IP3:8, IP4:8, Port:16, Rest/binary>>, Acc) ->
    read_peers(Rest, [#peer{ip = {IP1, IP2, IP3, IP4}, port = Port}|Acc]).
