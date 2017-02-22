-module(ahoy_piece_download).

-export([factory/1,
         start_link/4,
         completed_block/3,
         stop/1]).

-export_type([piece_dl/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type piece_dl() :: pid().
-type torrent_download() :: pid().
-type peer() :: pid().

-record(state, {torrent_download :: torrent_download(),
                piece_index :: ahoy_piece:piece_index(),
                piece :: ahoy_piece:piece(),
                peer :: peer(),
                last_piece}).

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
    next_request(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(request, State=#state{piece_index=PieceIndex, piece=Piece, peer=Peer}) ->
    case ahoy_piece:pop_missing_block(Piece) of
        {ok, {BlockOffset, BlockSize, _}} ->
            ahoy_peer:download(Peer, self(), PieceIndex, BlockOffset, BlockSize);
        false ->
            ok
    end,
    {noreply, State};
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
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, "Unknown message", State}.

handle_info(_Info, State) ->
    {stop, "Unknown message", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Proceed to next request.
next_request() ->
    gen_server:cast(self(), request).
