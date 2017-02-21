%%% @doc The responsibility of this module is to write binary data at specified
%%% position in a file.
-module(ahoy_file_writer).

-behaviour(gen_server).

-export([start_link/1,
         write/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {file}).

start_link(Path) ->
    gen_server:start_link(?MODULE, [Path], []).

%% @doc Write specified data at specified position in file.
-spec write(pid(), integer(), binary()) -> ok.
write(Pid, Position, Bin) ->
    gen_server:call(Pid, {write, Position, Bin}).

init([Path]) ->
    {ok, File} = file:open(Path, [write]),
    State = #state{file=File},
    {ok, State}.

handle_call({write, Position, Bin}, _From, State=#state{file=File}) ->
    {ok, _} = file:position(File, {bof, Position}),
    ok = file:write(File, Bin),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, #state{file=File}) ->
    file:close(File),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
