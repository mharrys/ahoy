-module(skur_tracker).

-export([start_link/7, get_peers/4]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("skur_metainfo.hrl").
-include_lib("skur_peer.hrl").

-record(state, {url,
                peers = [],
                complete,
                incomplete,
                uploaded,
                downloaded,
                left}).

start_link(Meta, InfoHash, PeerId, Port, Up, Down, Left) ->
    gen_server:start_link(?MODULE, [Meta, InfoHash, PeerId, Port, Up, Down, Left], []).

get_peers(Pid, Up, Down, Left) ->
    gen_server:call(Pid, {get_peers, Up, Down, Left}).

init([Meta, InfoHash, PeerId, Port, Up, Down, Left]) ->
    gen_server:cast(self(), update),
    Url = io_lib:format(
        "~s?info_hash=~s&peer_id=~s&port=~b&compact=1",
        [Meta#metainfo.announce,
         skur_util:encode_url(InfoHash),
         PeerId,
         Port]),
    {ok, #state{url = Url, left = Left, uploaded = Up, downloaded = Down}}.

handle_call({get_peers, Up, Down, Left}, _From, State) ->
    NewState = State#state{uploaded = Up, downloaded = Down, left = Left},
    {reply, State#state.peers, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(update, State) ->
    Url = io_lib:format(
        "~s&uploaded=~b&downloaded=~b&left=~b",
        [State#state.url,
         State#state.uploaded,
         State#state.downloaded,
         State#state.left]),
    {ok, {_, _, Body}} = httpc:request(lists:flatten(Url)),
    {dict, Resp} = bdecode:decode(list_to_binary(Body)),
    {_, RawPeers} = lists:keyfind(<<"peers">>, 1, Resp),
    {_, Interval} = lists:keyfind(<<"interval">>, 1, Resp),
    {_, Complete} = lists:keyfind(<<"complete">>, 1, Resp),
    {_, Incomplete} = lists:keyfind(<<"incomplete">>, 1, Resp),
    Peers = read_peers(RawPeers),
    timer:apply_after(Interval * 1000, gen_server, cast, [self(), update]),
    {noreply, State#state{peers = Peers,
                          complete = Complete,
                          incomplete = Incomplete}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

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
