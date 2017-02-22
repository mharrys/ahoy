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

-export([factory/1,
         start_link/1,
         set/2,
         is_set/2,
         is_empty/1,
         replace/2,
         raw_bitfield/1]).

-export_type([bitfield/0,
              bitfield_index/0,
              raw_bitfield/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type bitfield() :: pid().
-type bitfield_index() :: non_neg_integer().
-type raw_bitfield() :: binary().

-record(state, {bitfield :: binary()}).

%% @doc Factory function for creating a bitfield.
factory(NumBits) ->
    fun () ->
        start_link(NumBits)
    end.

%% @doc Start linked bitfield process.
start_link(NumBits) ->
    gen_server:start_link(?MODULE, [NumBits], []).

%% @doc Set the nth index in the bitfield.
-spec set(bitfield(), bitfield_index()) -> ok.
set(Ref, Index) ->
    gen_server:cast(Ref, {set, Index}).

%% @doc Validate if the nth index in the bitfield is set.
-spec is_set(bitfield(), bitfield_index()) -> boolean().
is_set(Ref, Index) ->
    gen_server:call(Ref, {is_set, Index}).

%% @doc True if no bit is set, false otherwise.
-spec is_empty(bitfield()) -> boolean().
is_empty(Ref) ->
    gen_server:call(Ref, is_empty).

%% @doc Replace raw bitfield with specified raw bitfield.
-spec replace(bitfield(), raw_bitfield()) -> ok.
replace(Ref, RawBitfield) ->
    gen_server:cast(Ref, {replace, RawBitfield}).

%% @doc Return raw bitfield.
-spec raw_bitfield(bitfield()) -> raw_bitfield().
raw_bitfield(Ref) ->
    gen_server:call(Ref, raw_bitfield).

init([NumBits]) ->
    Align = round(NumBits / 8) * 8,
    Bitfield = <<0:Align>>,
    State = #state{bitfield=Bitfield},
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
    Bitfield2 = <<Pad:Index, 1:1, T/bitstring>>,
    State2 = State#state{bitfield=Bitfield2},
    {noreply, State2};
handle_cast({replace, Bitfield2}, State=#state{bitfield=Bitfield}) ->
    true = bit_size(Bitfield2) =:= bit_size(Bitfield),
    State2 = State#state{bitfield=Bitfield2},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Validate if binary consist only of zeroes.
only_zero(<<>>) ->
    true;
only_zero(<<0:8, T/binary>>) ->
    only_zero(T);
only_zero(_) ->
    false.
