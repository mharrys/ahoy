%% Describes info dictionary in "metainfo"
-record(info, {name :: string(),
               length :: non_neg_integer(),
               piece_length :: non_neg_integer(),
               private = false :: boolean(),
               pieces :: binary()}).

-type info() :: #info{}.
-type datetime() :: calendar:datetime().

%% Describes metainfo file structure for a torrent file
-record(metainfo, {announce :: string(),
                   comment :: string(),
                   created_by :: string(),
                   creation_date :: datetime(),
                   encoding :: string(),
                   info :: info(),
                   info_hash :: binary()}).

-type metainfo() :: #metainfo{}.
