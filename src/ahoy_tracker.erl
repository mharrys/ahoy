-module(ahoy_tracker).

-export([start_link/2,
         start_link/3,
         get_last_response/1,
         get_peers/1]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("ahoy_metainfo.hrl").
-include_lib("ahoy_tracker_progress.hrl").
-include_lib("ahoy_tracker_response.hrl").

-record(state, {meta :: metainfo(),
                port :: port_number(),
                progress :: tracker_progress(),
                response :: tracker_response()}).

start_link(Meta, Port) ->
    Info = Meta#metainfo.info,
    Progress = #tracker_progress{
        uploaded = 0,
        downloaded = 0,
        left = Info#info.length
    },
    gen_server:start_link(?MODULE, [Meta, Port, Progress], []).

start_link(Meta, Port, Progress) ->
    gen_server:start_link(?MODULE, [Meta, Port, Progress], []).

%% @doc Return latest response recieved from tracker.
-spec get_last_response(pid()) -> tracker_response().
get_last_response(Pid) ->
    gen_server:call(Pid, get_last_response).

%% @doc Return peers from latest response recieved from tracker.
-spec get_peers(pid()) -> list(peer_address()).
get_peers(Pid) ->
    gen_server:call(Pid, get_peers).

init([Meta, Port, Progress]) ->
    State = #state{meta = Meta, port = Port, progress = Progress},
    gen_server:cast(self(), update),
    {ok, State}.

handle_call(get_last_response, _From, State=#state{response=Resp}) ->
    {reply, Resp, State};
handle_call(get_peers, _From, State=#state{response=Resp}) ->
    {reply, Resp#tracker_response.peers, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(update, State=#state{meta=Meta, port=Port, progress=Progress}) ->
    Resp = request_peers(Meta, Port, Progress),
    Interval = Resp#tracker_response.interval,
    timer:apply_after(Interval * 1000, gen_server, cast, [self(), update]),
    {noreply, State#state{response = Resp}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Request tracker to provide a list of active peers.
request_peers(Meta, Port, Progress) ->
    Url = ahoy_tracker_message:encode_request(Meta, Port, Progress),
    {ok, {_, _, Body}} = httpc:request(lists:flatten(Url)),
    ahoy_tracker_message:decode_response(Body).
