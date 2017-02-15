-module(ahoy_peer_message).

-export([encode_handshake/2,
         encode_keep_alive/0,
         encode_choke/0,
         encode_unchoke/0,
         encode_interested/0,
         encode_not_interested/0,
         encode_have/1,
         encode_bitfield/1,
         encode_request/3,
         encode_block/3,
         encode_cancel/3,
         decode_messages/1,
         decode_message/1]).

%% Encode peer wire protocol handshake message.
encode_handshake(InfoHash, PeerId) when is_list(PeerId) ->
    encode_handshake(InfoHash, list_to_binary(PeerId));
encode_handshake(<<InfoHash:20/binary>>, <<PeerId:20/binary>>) ->
    PStrLen = <<19>>,
    PStr = <<"BitTorrent protocol">>,
    Reserved = <<0:8/unit:8>>,
    <<PStrLen/binary,
      PStr/binary,
      Reserved/binary,
      InfoHash/binary,
      PeerId/binary>>.

encode_keep_alive() ->
    <<0:32>>.

encode_choke() ->
    encode(1, 0).

encode_unchoke() ->
    encode(1, 1).

encode_interested() ->
    encode(1, 2).

encode_not_interested() ->
    encode(1, 3).

encode_have(PieceIndex) ->
    encode(5, 4, <<PieceIndex:32>>).

%% Encode peer wire protocol message for bitfield, where Bitfield is expected
%% to be a binary value.
encode_bitfield(Bitfield) ->
    encode(1 + byte_size(Bitfield), 5, Bitfield).

encode_request(Index, Begin, Length) ->
    encode(13, 6, <<Index:32, Begin:32, Length:32>>).

%% Encode peer wire protocol message for block, where Block is expected to be
%% a binary value.
encode_block(Index, Begin, Block) ->
    encode(9 + byte_size(Block), 7, <<Index:32, Begin:32, Block/binary>>).

encode_cancel(Index, Begin, Length) ->
    encode(13, 8, <<Index:32, Begin:32, Length:32>>).

encode(Length, Id) ->
    <<Length:32, Id:8>>.
encode(Length, Id, Payload) ->
    <<Length:32, Id:8, Payload/binary>>.

%% Decode zero or more peer wire protocol message(s).
decode_messages(Msg) ->
    decode_messages(Msg, []).

decode_messages(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
decode_messages(Rest, [incomplete|Acc]) ->
    {lists:reverse(Acc), Rest};
decode_messages(Msg, Acc) ->
    {D, Rest} = decode_message(Msg),
    decode_messages(Rest, [D|Acc]).

%% Decode peer wire protocol message.
decode_message(<<19,
                 "BitTorrent protocol",
                 _Reserved:8/binary,
                 InfoHash:20/binary,
                 PeerId:20/binary,
                 T/binary>>) ->
    {{handshake, InfoHash, PeerId}, T};
decode_message(<<0:32, T/binary>>) ->
    {keep_alive, T};
decode_message(<<Length:32, Payload:Length/binary, T/binary>>) ->
    {decode(Payload), T};
decode_message(<<Length:32, T/binary>>) when Length > byte_size(T) ->
    {incomplete, <<Length:32, T/binary>>}.

decode(<<0:8>>) ->
    choke;
decode(<<1:8>>) ->
    unchoke;
decode(<<2:8>>) ->
    interested;
decode(<<3:8>>) ->
    not_interested;
decode(<<4:8, PieceIndex:32>>) ->
    {have, PieceIndex};
decode(<<5:8, Bitfield/binary>>) ->
    {bitfield, Bitfield};
decode(<<6:8, Index:32, Begin:32, Length:32>>) ->
    {request, Index, Begin, Length};
decode(<<7:8, Index:32, Begin:32, Block/binary>>) ->
    {block, Index, Begin, Block};
decode(<<8:8, Index:32, Begin:32, Length:32>>) ->
    {cancel, Index, Begin, Length}.
