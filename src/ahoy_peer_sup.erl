%%% @doc The responsibility of this module is to start and supervise remote
%%% peers. Every peer is highly volatile and disposable.
-module(ahoy_peer_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/5]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_child(Ref, Address, InfoHash, Bitfield, PeerActivity) ->
    Args = [Address, InfoHash, Bitfield, PeerActivity],
    supervisor:start_child(Ref, Args).

init([]) ->
    SupFlags = {simple_one_for_one, 0, 1},
    Child = {
        peer,
        {ahoy_peer, start_link, []},
        temporary,
        brutal_kill,
        worker,
        [ahoy_peer]
    },
    {ok, {SupFlags, [Child]}}.
