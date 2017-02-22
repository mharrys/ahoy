%%% @doc The responsibility of this module is to monitor peers that have
%%% successfully sent their bitfield or at least one piece index.
%%% @end
-module(ahoy_peer_activity).

-export([start_link/2,
         new_piece_index/3,
         new_bitfield/3,
         current_activity/1]).

-export_type([ref/0,
              activity/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type ref() :: pid().
-type activity() :: list({ahoy_peer:peer(), ahoy_bitfield:bitfield()}).

-record(state, {piece_stat,
                create_bitfield,
                activity :: activity()}).

start_link(PieceStat, CreateBitfield) ->
    gen_server:start_link(?MODULE, [PieceStat, CreateBitfield], []).

%% @doc Notify about new piece index from peer.
-spec new_piece_index(ref(), ahoy_peer:peer(), ahoy_piece:piece_index()) -> ok.
new_piece_index(Ref, Peer, PieceIndex) ->
    gen_server:cast(Ref, {new_piece_index, Peer, PieceIndex}).

%% @doc Notify about new raw bitfield from peer.
-spec new_bitfield(ref(), ahoy_peer:peer(), ahoy_bitfield:raw_bitfield()) -> ok.
new_bitfield(Ref, Peer, RawBitfield) ->
    gen_server:cast(Ref, {new_bitfield, Peer, RawBitfield}).

%% @doc Return current activity.
-spec current_activity(ref()) -> activity().
current_activity(Ref) ->
    gen_server:call(Ref, current_activity).

init([PieceStat, CreateBitfield]) ->
    State = #state{piece_stat=PieceStat, create_bitfield=CreateBitfield, activity=[]},
    {ok, State}.

handle_call(current_activity, _From, State=#state{activity=Activity}) ->
    Reply = Activity,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({new_piece_index, Peer, PieceIndex}, State=#state{piece_stat=PieceStat,
                                                              create_bitfield=CreateBitfield,
                                                              activity=Activity}) ->
    ahoy_piece_stat:add_piece_index(PieceStat, PieceIndex),
    Activity2 = ensure_added(Peer, CreateBitfield, Activity),
    lists:foreach(new_piece_index(Peer, PieceIndex), Activity2),
    State2 = State#state{activity=Activity2},
    {noreply, State2};
handle_cast({new_bitfield, Peer, RawBitfield}, State=#state{piece_stat=PieceStat,
                                                            create_bitfield=CreateBitfield,
                                                            activity=Activity}) ->
    ahoy_piece_stat:add_bitfield(PieceStat, RawBitfield),
    Activity2 = ensure_added(Peer, CreateBitfield, Activity),
    lists:foreach(new_bitfield(Peer, RawBitfield), Activity2),
    State2 = State#state{activity=Activity2},
    {noreply, State2};
handle_cast(_Request, State) ->
    {stop, "Unknown message", State}.

handle_info({'DOWN', _MonitorRef, process, Peer, _Reason}, State=#state{piece_stat=PieceStat,
                                                                        activity=Activity}) ->
    Activity3 = case lists:keytake(Peer, 1, Activity) of
        {value, {Peer, Bitfield}, Activity2} ->
            RawBitfield = ahoy_bitfield:raw_bitfield(Bitfield),
            ahoy_piece_stat:remove_bitfield(PieceStat, RawBitfield),
            Activity2
    end,
    State2 = State#state{activity=Activity3},
    {noreply, State2};
handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Ensure specified peer activity is monitored.
ensure_added(Peer, CreateBitfield, Activity) ->
    case lists:keymember(Peer, 1, Activity) of
        true ->
            Activity;
        false ->
            erlang:monitor(process, Peer),
            {ok, Bitfield} = CreateBitfield(),
            [{Peer, Bitfield}|Activity]
    end.

%% Set piece index in specified peer's bitfield.
new_piece_index(Peer, PieceIndex) ->
    fun ({Peer2, Bitfield}) ->
            case Peer =:= Peer2 of
                true ->
                    ahoy_bitfield:set(Bitfield, PieceIndex),
                    ok;
                _ ->
                    ok
            end
    end.

%% Replace specified peer's raw bitfield.
new_bitfield(Peer, RawBitfield) ->
    fun ({Peer2, Bitfield}) ->
            case Peer =:= Peer2 of
                true ->
                    ahoy_bitfield:replace(Bitfield, RawBitfield),
                    ok;
                _ ->
                    ok
            end
    end.
