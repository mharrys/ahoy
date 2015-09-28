-module(peer_message_tests).

-include_lib("eunit/include/eunit.hrl").

encode_handshake_test() ->
    InfoHash = <<"aaaaaaaaaaaaaaaaaaaa">>,
    PeerId = <<"bbbbbbbbbbbbbbbbbbbb">>,
    Handshake = peer_message:encode_handshake(InfoHash, PeerId),
    ?assertEqual(68, byte_size(Handshake)),
    <<PStrLen:8, PStr:19/binary, Reserved:8/binary, T/binary>> = Handshake,
    ?assertEqual(19, PStrLen),
    ?assertEqual(<<"BitTorrent protocol">>, PStr),
    ?assertEqual(<<0:8/unit:8>>, Reserved),
    <<InfoHash2:20/binary, PeerId2:20/binary, T2/binary>> = T,
    ?assertEqual(InfoHash, InfoHash2),
    ?assertEqual(PeerId, PeerId2),
    ?assertEqual(<<>>, T2).

decode_handshake_test() ->
    Invalid = <<"invalid">>,
    ?assertError(function_clause, peer_message:decode_handshake(Invalid)),
    PStrLen = 19,
    PStr = <<"BitTorrent protocol">>,
    Reserved = <<0:8/unit:8>>,
    InfoHash = <<"aaaaaaaaaaaaaaaaaaaa">>,
    PeerId = <<"bbbbbbbbbbbbbbbbbbbb">>,
    Valid = <<PStrLen, PStr/binary, Reserved/binary, InfoHash/binary, PeerId/binary>>,
    ?assertEqual({{InfoHash, PeerId}, <<>>}, peer_message:decode_handshake(Valid)),
    Tail = <<"foobar">>,
    Valid2 = <<Valid/binary, Tail/binary>>,
    ?assertEqual({{InfoHash, PeerId}, Tail}, peer_message:decode_handshake(Valid2)),
    Invalid2 = <<18, PStr/binary, Reserved/binary, InfoHash/binary, PeerId/binary>>,
    ?assertError(function_clause, peer_message:decode_handshake(Invalid2)),
    Invalid3 = <<PStrLen, "foobar", Reserved/binary, InfoHash/binary, PeerId/binary>>,
    ?assertError(function_clause, peer_message:decode_handshake(Invalid3)),
    Invalid4 = <<Invalid3/binary, Tail/binary>>,
    ?assertError(function_clause, peer_message:decode_handshake(Invalid4)),
    Invalid5 = <<PStrLen, PStr/binary, "foobar", InfoHash/binary, PeerId/binary>>,
    ?assertError(function_clause, peer_message:decode_handshake(Invalid5)),
    % valid in size, but invalid order (which we can't know)
    Valid3 = <<PStrLen, PStr/binary, Reserved/binary, PeerId/binary, InfoHash/binary>>,
    ?assertEqual({{PeerId, InfoHash}, <<>>}, peer_message:decode_handshake(Valid3)).

encode_empty_payload_messages_test() ->
    ?assertEqual(<<0, 0, 0, 0>>, peer_message:encode_keep_alive()),
    ?assertEqual(<<0, 0, 0, 1, 0>>, peer_message:encode_choke()),
    ?assertEqual(<<0, 0, 0, 1, 1>>, peer_message:encode_unchoke()),
    ?assertEqual(<<0, 0, 0, 1, 2>>, peer_message:encode_interested()),
    ?assertEqual(<<0, 0, 0, 1, 3>>, peer_message:encode_not_interested()).

encode_have_test() ->
    ?assertEqual(<<0, 0, 0, 5, 4, 0, 0, 0, 0>>, peer_message:encode_have(0)),
    ?assertEqual(<<0, 0, 0, 5, 4, 0, 0, 0, 42>>, peer_message:encode_have(42)),
    ?assertEqual(<<0, 0, 0, 5, 4, 1337:32>>, peer_message:encode_have(1337)).

encode_bitfield_test() ->
    BF = <<>>,
    ?assertEqual(<<0, 0, 0, 1, 5>>, peer_message:encode_bitfield(BF)),
    BF1 = <<0>>,
    ?assertEqual(<<0, 0, 0, 2, 5, BF1/binary>>, peer_message:encode_bitfield(BF1)),
    BF2 = <<1,2,3,4,5>>,
    ?assertEqual(<<0, 0, 0, 6, 5, BF2/binary>>, peer_message:encode_bitfield(BF2)).

encode_request_test() ->
    R = peer_message:encode_request(0, 0, 0),
    ?assertEqual(<<0, 0, 0, 13, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>, R),
    R2 = peer_message:encode_request(42, 13, 37),
    ?assertEqual(<<0, 0, 0, 13, 6, 0, 0, 0, 42, 0, 0, 0, 13, 0, 0, 0, 37>>, R2).

encode_piece_test() ->
    Piece = <<"foobar">>,
    P = peer_message:encode_piece(0, 0, Piece),
    ?assertEqual(<<0, 0, 0, 15, 7, 0, 0, 0, 0, 0, 0, 0, 0, Piece/binary>>, P),
    P2 = peer_message:encode_piece(42, 1337, Piece),
    ?assertEqual(<<0, 0, 0, 15, 7, 42:32, 1337:32, Piece/binary>>, P2).

encode_cancel_test() ->
    C = peer_message:encode_cancel(0, 0, 0),
    ?assertEqual(<<0, 0, 0, 13, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>, C),
    C2 = peer_message:encode_cancel(42, 13, 37),
    ?assertEqual(<<0, 0, 0, 13, 8, 42:32, 13:32, 37:32>>, C2).

decode_messages_no_payload_test() ->
    ?assertEqual([], peer_message:decode_messages(<<>>)),
    ?assertError(function_clause, peer_message:decode_messages(<<"foobar">>)),
    ?assertEqual([keep_alive], peer_message:decode_messages(<<0:32>>)),
    ?assertError(
        function_clause,
        peer_message:decode_messages(<<0:32, "foobar">>)),
    ?assertError(
        function_clause,
        peer_message:decode_messages(<<0:32, "foobar", 1:32, 2>>)),
    ?assertEqual(
        [keep_alive, interested],
        peer_message:decode_messages(<<0:32, 1:32, 2>>)).

decode_messages_test() ->
    M = <<15:32, 7, 42:32, 1337:32, "foobarbaz">>,
    ?assertError(function_clause, peer_message:decode_messages(M)),
    M2 = <<1:32, 3, 13:32, 8, 1000:32, 2000:32, 3000:32>>,
    ?assertEqual(
        [not_interested, {cancel, 1000, 2000, 3000}],
        peer_message:decode_messages(M2)),
    M3 = <<5000:32, 42>>,
    ?assertError(function_clause, peer_message:decode_messages(M3)),
    M4 = <<1:32, 42>>,
    ?assertError(function_clause, peer_message:decode_messages(M4)).

decode_message_no_payload_test() ->
    T = <<"foobar">>,
    ?assertEqual({keep_alive, <<>>}, peer_message:decode_message(<<0:32>>)),
    ?assertEqual({keep_alive, <<0:32>>}, peer_message:decode_message(<<0:32, 0:32>>)),
    ?assertEqual({keep_alive, T}, peer_message:decode_message(<<0:32, T/binary>>)),
    ?assertEqual({choke, <<>>}, peer_message:decode_message(<<1:32, 0>>)),
    ?assertEqual({choke, <<1:24>>}, peer_message:decode_message(<<1:32, 1:32>>)),
    ?assertEqual({choke, T}, peer_message:decode_message(<<1:32, 0, T/binary>>)),
    ?assertEqual({unchoke, <<>>}, peer_message:decode_message(<<1:32, 1>>)),
    ?assertEqual({unchoke, T}, peer_message:decode_message(<<1:32, 1, T/binary>>)),
    ?assertEqual({interested, <<>>}, peer_message:decode_message(<<1:32, 2>>)),
    ?assertEqual({interested, T}, peer_message:decode_message(<<1:32, 2, T/binary>>)),
    ?assertEqual({not_interested, <<>>}, peer_message:decode_message(<<1:32, 3>>)),
    ?assertEqual({not_interested, T}, peer_message:decode_message(<<1:32, 3, T/binary>>)),
    % valid length, but unknown id
    ?assertError(function_clause, peer_message:decode_message(<<1:32, 20>>)),
    ?assertError(function_clause, peer_message:decode_message(<<1:32, 20, T/binary>>)).

decode_message_test() ->
    T = <<"foobar">>,
    ?assertEqual({{have, 1337}, <<>>}, peer_message:decode_message(<<5:32, 4, 1337:32>>)),
    ?assertEqual(
        {{have, 1337}, T},
        peer_message:decode_message(<<5:32, 4, 1337:32, T/binary>>)),
    B = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>,
    ?assertEqual(
        {{bitfield, B}, <<>>},
        peer_message:decode_message(<<11:32, 5, B/binary>>)),
    ?assertEqual(
        {{bitfield, B}, T},
        peer_message:decode_message(<<11:32, 5, B/binary, T/binary>>)),
    ?assertEqual(
        {{request, 42, 13, 37}, <<>>},
        peer_message:decode_message(<<13:32, 6, 42:32, 13:32, 37:32>>)),
    ?assertEqual(
        {{request, 42, 13, 37}, T},
        peer_message:decode_message(<<13:32, 6, 42:32, 13:32, 37:32, T/binary>>)),
    P = <<"baz">>,
    ?assertEqual(
        {{piece, 13, 37, P}, <<>>},
        peer_message:decode_message(<<12:32, 7, 13:32, 37:32, P/binary>>)),
    ?assertEqual(
        {{piece, 13, 37, P}, T},
        peer_message:decode_message(<<12:32, 7, 13:32, 37:32, P/binary, T/binary>>)),
    ?assertEqual(
        {{cancel, 42, 13, 37}, <<>>},
        peer_message:decode_message(<<13:32, 8, 42:32, 13:32, 37:32>>)),
    ?assertEqual(
        {{cancel, 42, 13, 37}, T},
        peer_message:decode_message(<<13:32, 8, 42:32, 13:32, 37:32, T/binary>>)).
