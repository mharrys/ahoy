-define(BLOCK_SIZE, 16384).

-type block_index() :: non_neg_integer().
-type block_offset() :: non_neg_integer().
-type block_size() :: non_neg_integer().
-type block_data() :: binary() | atom().
-type block() :: {block_index(), block_data()}.
