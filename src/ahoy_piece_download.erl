-module(ahoy_piece_download).

-export([start_link/5,
         completed_block/3]).

-export_type([piece_dl/0]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("ahoy_block.hrl").

-type piece_dl() :: pid().
-type torrent_download() :: pid().
-type peer() :: pid().

-record(state, {torrent_download :: torrent_download(),
                piece_index :: ahoy_piece:piece_index(),
                piece :: ahoy_piece:piece(),
                peer :: peer(),
                last_piece}).

start_link(TorrentDownload, PieceIndex, PieceLength, Peer, LastPiece) ->
    gen_server:start_link(?MODULE, [TorrentDownload, PieceIndex, PieceLength, Peer, LastPiece], []).

%% @doc Download request response with completed block data.
-spec completed_block(piece_dl(), ahoy_piece:piece_index(), ahoy_piece:block()) -> ok.
completed_block(Ref, PieceIndex, Block) ->
    gen_server:cast(Ref, {completed, PieceIndex, Block}).

init([TorrentDownload, PieceIndex, PieceLength, Peer, LastPiece]) ->
    {LastPieceIndex, _, _} = LastPiece,
    {ok, Piece} = case PieceIndex =:= LastPieceIndex of
        true ->
            ahoy_piece:start_link(PieceLength, ?BLOCK_SIZE, LastPiece);
        false ->
            ahoy_piece:start_link(PieceLength)
    end,
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
        {ok, {{BlockIndex, _}, BlockSize}} ->
            ahoy_peer:download(Peer, self(), PieceIndex, BlockIndex, BlockSize);
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
            ahoy_torrent_download:completed_piece_download(Download, PieceIndex, RawPiece);
        false ->
            next_request()
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Proceed to next request.
next_request() ->
    gen_server:cast(self(), request).
