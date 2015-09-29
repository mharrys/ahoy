%% Describes metainfo file structure for a torrent file
-record(metainfo, {info,
                   info_hash,
                   announce,
                   announce_list,
                   creation_date,
                   httpseeds,
                   comment,
                   created_by,
                   encoding}).

%% Describes info dictionary in "metainfo"
-record(info, {piece_length,
               pieces,
               private,
               name,
               length,
               md5sum,
               files}).

%% Describes one entry in "files" (multiple file mode)
-record(filesinfo, {length,
                    md5sum,
                    path}).
