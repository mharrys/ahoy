-module(ahoy_piece_download).

-export([factory/1,
         start_link/4,
         completed_block/3,
         time_since_request/1,
         stop/1]).

-export_type([piece_dl/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(HEARTBEAT, 500).

-type piece_dl() :: pid().
-type torrent_download() :: pid().
-type peer() :: pid().

-record(state, {torrent_download :: torrent_download(),
                piece_index :: ahoy_piece:piece_index(),
                piece :: ahoy_piece:piece(),
                peer :: peer(),
                time_waited = 0 :: non_neg_integer()}).

%% @doc Factory function for creating a piece download.
factory(PieceFactory) ->
    fun (TorrentDownload, PieceIndex, Peer) ->
        {ok, Piece} = PieceFactory(PieceIndex),
        start_link(TorrentDownload, PieceIndex, Piece, Peer)
    end.

start_link(TorrentDownload, PieceIndex, Piece, Peer) ->
    gen_server:start_link(?MODULE, [TorrentDownload, PieceIndex, Piece, Peer], []).

%% @doc Download request response with completed block data.
-spec completed_block(piece_dl(), ahoy_piece:piece_index(), ahoy_piece:block()) -> ok.
completed_block(Ref, PieceIndex, Block) ->
    gen_server:cast(Ref, {completed, PieceIndex, Block}).

%% @doc Return in milliseconds the time since the last request was made.
-spec time_since_request(piece_dl()) -> non_neg_integer().
time_since_request(Ref) ->
    gen_server:call(Ref, time_since_request).

%% @doc Stop piece download.
stop(Ref) ->
    gen_server:stop(Ref).

init([TorrentDownload, PieceIndex, Piece, Peer]) ->
    State = #state{
        torrent_download = TorrentDownload,
        piece_index = PieceIndex,
        piece = Piece,
        peer = Peer
    },
    heartbeat(),
    next_request(),
    {ok, State}.

handle_call(time_since_request, _From, State=#state{time_waited=Request}) ->
    Reply = Request,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(heartbeat, State=#state{time_waited=Waited}) ->
    heartbeat(),
    Waited2 = Waited + ?HEARTBEAT, % No need for exact measure
    State2 = State#state{time_waited=Waited2},
    {noreply, State2};
handle_cast(request, State=#state{piece_index=PieceIndex, piece=Piece, peer=Peer}) ->
    case ahoy_piece:pop_missing_block(Piece) of
        {ok, {BlockOffset, BlockSize, _}} ->
            ahoy_peer:download(Peer, self(), PieceIndex, BlockOffset, BlockSize);
        false ->
            ok
    end,
    State2 = State#state{time_waited=0},
    {noreply, State2};
handle_cast({completed, PieceIndex, Block}, State=#state{torrent_download=Download,
                                                         piece_index=PieceIndex,
                                                         piece=Piece}) ->
    ahoy_piece:add_completed_block(Piece, Block),
    % check if we are done
    case ahoy_piece:raw_piece(Piece) of
        {ok, RawPiece} ->
            ahoy_torrent_download:completed_piece(Download, PieceIndex, RawPiece);
        false ->
            next_request()
    end,
    State2 = State#state{time_waited=0},
    {noreply, State2};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

heartbeat() ->
    timer:apply_after(?HEARTBEAT, gen_server, cast, [self(), heartbeat]).

%% Proceed to next request.
next_request() ->
    gen_server:cast(self(), request).
