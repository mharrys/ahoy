%% Describes metainfo file structure for a torrent file
-record(metainfo, {announce,
                   comment,
                   created_by,
                   creation_date,
                   encoding,
                   info,
                   info_hash}).

%% Describes info dictionary in "metainfo"
-record(info, {name,
               length,
               piece_length,
               private,
               pieces}).
