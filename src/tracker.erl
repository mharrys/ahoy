-module(tracker).
-export([start_link/7]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("skur/include/metainfo.hrl").
-include_lib("skur/include/peer.hrl").
-record(state, { metainfo
               , peer_id
               , port
               , info_hash
               , peers = []
               , complete
               , incomplete
               , uploaded
               , downloaded
               , left
               }).

start_link(Meta, InfoHash, PeerId, Port, Up, Down, Left) ->
    gen_server:start_link(?MODULE, [Meta, InfoHash, PeerId, Port, Up, Down, Left], []).

init([Meta, InfoHash, PeerId, Port, Up, Down, Left]) ->
    gen_server:cast(self(), update),
    {ok, #state{metainfo = Meta, peer_id = PeerId, info_hash = InfoHash,
                port = Port, left = Left, uploaded = Up, downloaded = Down}}.

handle_call({get_peers, Up, Down, Left}, _From, State) ->
    NewState = State#state{uploaded = Up, downloaded = Down, left = Left},
    {reply, State#state.peers, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(update, State) ->
    Url = create_url(State#state.metainfo#metainfo.announce,
        [{"info_hash", State#state.info_hash},
         {"peer_id", State#state.peer_id},
         {"port", State#state.port},
         {"uploaded", State#state.uploaded},
         {"downloaded", State#state.downloaded},
         {"left", State#state.left},
         {"compact", 1}, {"no_peer_id", 1}]),
    {ok, {_, _, Body}} = httpc:request(Url),
    {dict, Resp} = bdecode:decode(list_to_binary(Body)),
    {_, RawPeers} = lists:keyfind(<<"peers">>, 1, Resp),
    {_, Interval} = lists:keyfind(<<"interval">>, 1, Resp),
    {_, Complete} = lists:keyfind(<<"complete">>, 1, Resp),
    {_, Incomplete} = lists:keyfind(<<"incomplete">>, 1, Resp),
    Peers = read_peers(RawPeers),
    timer:apply_after(Interval * 1000, gen_server, cast, [self(), update]),
    {noreply, State#state{peers = Peers, complete = Complete,
                          incomplete = Incomplete}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

create_url(Url, Parameters) ->
    create_url(Url, Parameters, "").
create_url(Url, [], Acc) ->
    Url ++ "?" ++ Acc;
create_url(Url, [{Key, Value}|Parameters], Acc) when is_integer(Value) ->
    NewAcc = Acc ++ http_uri:encode(Key) ++ "=" ++ integer_to_list(Value) ++ "&",
    create_url(Url, Parameters, NewAcc);
create_url(Url, [{Key, Value}|Parameters], Acc) when is_binary(Value) ->
    NewAcc = Acc ++ http_uri:encode(Key) ++ "=" ++ urlencode(Value) ++ "&",
    create_url(Url, Parameters, NewAcc);
create_url(Url, [{Key, Value}|Parameters], Acc) ->
    NewAcc = Acc ++ http_uri:encode(Key) ++ "=" ++ http_uri:encode(Value) ++ "&",
    create_url(Url, Parameters, NewAcc).

urlencode(Bytes) ->
    urlencode(Bytes, "").
urlencode(<<>>, Acc) ->
    Acc;
urlencode(<<Byte:8, Rest/binary>>, Acc) when Byte < 16 ->
    urlencode(Rest, Acc ++ [$%|[$0|integer_to_list(Byte, 16)]]);
urlencode(<<Byte:8, Rest/binary>>, Acc) when Byte < 32; Byte >= 127 ->
    urlencode(Rest, Acc ++ [$%|integer_to_list(Byte, 16)]);
urlencode(<<Byte:8, Rest/binary>>, Acc) ->
    urlencode(Rest, Acc ++ [Byte]).

read_peers(Bytes) ->
    read_peers(Bytes, []).
read_peers(<<>>, Acc) ->
    Acc;
read_peers(<<IP1:8, IP2:8, IP3:8, IP4:8, Port:16, Rest/binary>>, Acc) ->
    read_peers(Rest, [#peer{ip = {IP1, IP2, IP3, IP4}, port = Port}|Acc]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.