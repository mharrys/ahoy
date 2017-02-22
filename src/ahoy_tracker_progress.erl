%%% @doc The responsibility of this module is to track the download/upload progress
%%% of a torrent.
%%% @end
-module(ahoy_tracker_progress).

-export([start_link/1,
         downloaded/2,
         uploaded/2,
         progress/1]).

-export_types([tracker_progress/0,
               progress/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {up = 0 :: bytes(),
                down = 0 :: bytes(),
                left = 0:: bytes()}).

-type tracker_progress() :: pid().
-type bytes() :: non_neg_integer().
-type progress() :: {bytes(), bytes(), bytes()}.

start_link(Left) ->
    gen_server:start_link(?MODULE, [Left], []).

%% @doc Add new downloaded bytes.
-spec downloaded(tracker_progress(), bytes()) -> ok.
downloaded(Ref, Bytes) ->
    gen_server:cast(Ref, {downloaded, Bytes}).

%% @doc Add new uploaded bytes.
-spec uploaded(tracker_progress(), bytes()) -> ok.
uploaded(Ref, Bytes) ->
    gen_server:cast(Ref, {uploaded, Bytes}).

%% @doc Return progress tuple.
-spec progress(tracker_progress()) -> progress().
progress(Ref) ->
    gen_server:call(Ref, progress).

init([Length]) ->
    State = #state{left=Length},
    {ok, State}.

handle_call(progress, _From, State=#state{up=Up, down=Down, left=Left}) ->
    Left2 = Left - Down,
    Reply = {Up, Down, Left2},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({downloaded, Bytes}, State=#state{down=Down, left=Left}) ->
    Down2 = Down + Bytes,
    Percent = round((Down2 / Left) * 100),
    io:format("~p% ~p/~p~n", [Percent, Down2, Left]),
    State2 = State#state{down=Down2},
    {noreply, State2};
handle_cast({uploaded, Bytes}, State=#state{up=Up}) ->
    Up = Up + Bytes,
    State2 = State#state{up=Up},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
