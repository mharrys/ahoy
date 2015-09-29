-module(skur_app).

-behaviour(application).

-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start(_StartType, _StartArgs) ->
    skur_sup:start_link().

stop(_State) ->
    ok.
