%%% @doc The bitfield module is encapsulating a binary representation of a
%%% series of bits. Each bit begins unset and can be set, and once a bit is
%%% set it can not be unset.
%%%
%%% In the BitTorrent protocol, each bit represent a piece where unset means
%%% the piece is missing and set means that the piece is downloaded and
%%% verified. It can be embedded in an encoded peer message and is usually
%%% sent once after the handshake is complete between two peers, but optional
%%% to send if all bits are unset.
%%%
%%% Since the main purpose of the bitfield is to be sent over a TCP socket, it
%%% is aligned as a multiple of 8 which means that the number of bits can be
%%% less or more than what is specified at start.
%%% @end
-module(ahoy_bitfield).

-export([start_link/1,
         set/2,
         is_set/2,
         is_empty/1,
         raw_bitfield/1]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {bitfield :: binary()}).

start_link(NumBits) ->
    gen_server:start_link(?MODULE, [NumBits], []).

%% @doc Set the nth index in the bitfield.
-spec set(pid(), non_neg_integer()) -> ok.
set(Pid, Index) ->
    gen_server:cast(Pid, {set, Index}).

%% @doc Validate if the nth index in the bitfield is set.
-spec is_set(pid(), non_neg_integer()) -> boolean().
is_set(Pid, Index) ->
    gen_server:call(Pid, {is_set, Index}).

%% @doc True if no bit is set, false otherwise.
-spec is_empty(pid()) -> boolean().
is_empty(Pid) ->
    gen_server:call(Pid, is_empty).

%% @doc Return raw bitfield.
-spec raw_bitfield(pid()) -> binary().
raw_bitfield(Pid) ->
    gen_server:call(Pid, raw_bitfield).

init([NumBits]) ->
    Align = round(NumBits / 8) * 8,
    Bitfield = <<0:Align>>,
    State = #state{bitfield = Bitfield},
    {ok, State}.

handle_call({is_set, Index}, _From, State=#state{bitfield=Bitfield}) ->
    <<_:Index, N:1, _/bitstring>> = Bitfield,
    Reply = N =:= 1,
    {reply, Reply, State};
handle_call(is_empty, _From, State=#state{bitfield=Bitfield}) ->
    Reply = only_zero(Bitfield),
    {reply, Reply, State};
handle_call(raw_bitfield, _From, State=#state{bitfield=Bitfield}) ->
    {reply, Bitfield, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({set, Index}, State=#state{bitfield=Bitfield}) ->
    <<Pad:Index, _:1, T/bitstring>> = Bitfield,
    NewBitfield = <<Pad:Index, 1:1, T/bitstring>>,
    {noreply, State#state{bitfield=NewBitfield}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

only_zero(<<>>) ->
    true;
only_zero(<<0:8, T/binary>>) ->
    only_zero(T);
only_zero(_) ->
    false.
