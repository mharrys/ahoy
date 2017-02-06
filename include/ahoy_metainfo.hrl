%% Describes metainfo file structure for a torrent file
-record(metainfo, {info,
                   info_hash,
                   announce,
                   creation_date,
                   comment,
                   created_by,
                   encoding}).

%% Describes info dictionary in "metainfo"
-record(info, {piece_length,
               pieces,
               private,
               name,
               length}).
