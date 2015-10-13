-module(skur_metainfo).

-export([new/1,
         single_file_mode/1,
         multiple_file_mode/1]).

-include_lib("skur_metainfo.hrl").

%% Create a new metainfo structure from specified Path.
new(Path) when is_list(Path) ->
    {dict, D} = skur_bdecode:decode(Path),
    parse(D, #metainfo{}).

%% Return true if single file mode, false otherwise.
single_file_mode(#metainfo{info=Info}) ->
    Info#info.files == undefined.

%% Return true if multiple file mode, false otherwise.
multiple_file_mode(Meta) ->
    not single_file_mode(Meta).

%% Parse skur_metainfo file structure
parse([{<<"info">>, {dict, Dict}}|T], Meta) ->
    Info = parse_info(Dict, #info{}),
    Encoded = skur_bencode:encode({dict, Dict}),
    Hash = crypto:hash(sha, Encoded),
    parse(T, Meta#metainfo{info=Info, info_hash=Hash});
parse([{<<"announce">>, Bin}|T], Meta) ->
    Announce = binary:bin_to_list(Bin),
    parse(T, Meta#metainfo{announce=Announce});
parse([{<<"announce-list">>, {list, List}}|T], Meta) ->
    AnnounceList = [binary:bin_to_list(X) || {list, [X]} <- List],
    parse(T, Meta#metainfo{announce_list=AnnounceList});
parse([{<<"creation date">>, Timestamp}|T], Meta) ->
    CreationDate = parse_timestamp(Timestamp),
    parse(T, Meta#metainfo{creation_date=CreationDate});
parse([{<<"httpseeds">>, {list, List}}|T], Meta) ->
    Httpseeds = [binary:bin_to_list(X) || {list, [X]} <- List],
    parse(T, Meta#metainfo{httpseeds=Httpseeds});
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
    io:format("Unknown skur_metainfo ~p~n", [H]),
    parse(T, Meta);
parse([], Meta) ->
    Meta.

%% Parse info dictionary (within skur_metainfo file structure)
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
parse_info([{<<"md5sum">>, MD5Sum}|T], Info) ->
    parse_info(T, Info#info{md5sum=MD5Sum});
parse_info([{<<"files">>, {list, List}}|T], Info) ->
    Files = [parse_files(X, #filesinfo{}) || {dict, X} <- List],
    parse_info(T, Info#info{files=Files});
parse_info([H|T], Info) ->
    io:format("Unknown info ~p~n", [H]),
    parse_info(T, Info);
parse_info([], Info) ->
    Info.

%% Parse info in multiple file mode (within info dictionary)
parse_files([{<<"length">>, Length}|T], Files) ->
    parse_files(T, Files#filesinfo{length=Length});
parse_files([{<<"md5sum">>, MD5Sum}|T], Files) ->
    parse_files(T, Files#filesinfo{md5sum=MD5Sum});
parse_files([{<<"path">>, {list, List}}|T], Files) ->
    Path = binary:bin_to_list(filename:join(List)),
    parse_files(T, Files#filesinfo{path=Path});
parse_files([H|T], Files) ->
    io:format("Unknown filesinfo ~p~n", [H]),
    parse_files(T, Files);
parse_files([], Files) ->
    Files.

%% Parse timestamp to datetime
parse_timestamp(Timestamp) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = Epoch + Timestamp,
    calendar:gregorian_seconds_to_datetime(Seconds).
