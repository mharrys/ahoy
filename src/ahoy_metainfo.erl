-module(ahoy_metainfo).

-export([new/1]).

-include_lib("ahoy_metainfo.hrl").

%% Create a new metainfo structure from specified Path.
new(Path) when is_list(Path) ->
    {dict, D} = ahoy_bdecode:decode(Path),
    parse(D, #metainfo{}).

%% Parse ahoy_metainfo file structure
parse([{<<"info">>, {dict, Dict}}|T], Meta) ->
    Info = parse_info(Dict, #info{}),
    Encoded = ahoy_bencode:encode({dict, Dict}),
    Hash = crypto:hash(sha, Encoded),
    parse(T, Meta#metainfo{info=Info, info_hash=Hash});
parse([{<<"announce">>, Bin}|T], Meta) ->
    Announce = binary:bin_to_list(Bin),
    parse(T, Meta#metainfo{announce=Announce});
parse([{<<"creation date">>, Timestamp}|T], Meta) ->
    CreationDate = parse_timestamp(Timestamp),
    parse(T, Meta#metainfo{creation_date=CreationDate});
parse([{<<"comment">>, Bin}|T], Meta) ->
    Comment = binary:bin_to_list(Bin),
    parse(T, Meta#metainfo{comment=Comment});
parse([{<<"created by">>, Bin}|T], Meta) ->
    CreatedBy = binary:bin_to_list(Bin),
    parse(T, Meta#metainfo{created_by=CreatedBy});
parse([{<<"encoding">>, Bin}|T], Meta) ->
    Encoding = binary:bin_to_list(Bin),
    parse(T, Meta#metainfo{encoding=Encoding});
parse([H|T], Meta) ->
    io:format("Unknown metainfo ~p~n", [H]),
    parse(T, Meta);
parse([], Meta) ->
    Meta.

%% Parse info dictionary (within ahoy_metainfo file structure)
parse_info([{<<"piece length">>, PieceLength}|T], Info) ->
    parse_info(T, Info#info{piece_length=PieceLength});
parse_info([{<<"pieces">>, Pieces}|T], Info) ->
    parse_info(T, Info#info{pieces=Pieces});
parse_info([{<<"private">>, Private}|T], Info) ->
    parse_info(T, Info#info{private=Private});
parse_info([{<<"name">>, Bin}|T], Info) ->
    Name = binary:bin_to_list(Bin),
    parse_info(T, Info#info{name=Name});
parse_info([{<<"length">>, Length}|T], Info) ->
    parse_info(T, Info#info{length=Length});
parse_info([H|T], Info) ->
    io:format("Unknown info ~p~n", [H]),
    parse_info(T, Info);
parse_info([], Info) ->
    Info.

%% Parse timestamp to datetime
parse_timestamp(Timestamp) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = Epoch + Timestamp,
    calendar:gregorian_seconds_to_datetime(Seconds).
