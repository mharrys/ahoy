-module(ahoy_tracker).

-export([start_link/4]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("ahoy_metainfo.hrl").
-include_lib("ahoy_tracker_response.hrl").

-type torrent() :: pid().

-record(state, {torrent :: torrent(),
                progress :: ahoy_tracker_progress:tracker_progress(),
                meta :: metainfo(),
                port :: port_number()}).

start_link(Torrent, Progress, Meta, Port) ->
    gen_server:start_link(?MODULE, [Torrent, Progress, Meta, Port], []).

init([Torrent, Progress, Meta, Port]) ->
    State = #state{torrent=Torrent, progress=Progress, meta=Meta, port=Port},
    gen_server:cast(self(), update),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(update, State=#state{torrent=Torrent, meta=Meta, port=Port,
        progress=Progress}) ->
    Resp = request_update(Meta, Port, Progress),
    Interval = Resp#tracker_response.interval,
    update(Interval),
    PeerAddresses = Resp#tracker_response.peer_addresses,
    ahoy_torrent:update_tracker_peers(Torrent, PeerAddresses),
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Call update after specified number of seconds.
update(Interval) ->
    timer:apply_after(Interval * 1000, gen_server, cast, [self(), update]).

%% Request update from tracker.
request_update(Meta, Port, Progress) ->
    Url = ahoy_tracker_message:encode_request(Meta, Port, Progress),
    {ok, {_, _, Body}} = httpc:request(lists:flatten(Url)),
    ahoy_tracker_message:decode_response(Body).
