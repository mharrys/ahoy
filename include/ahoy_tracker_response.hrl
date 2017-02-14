-include_lib("ahoy_peer_address.hrl").

-type second() :: integer().

%% Describes tracker response dictionary
-record(tracker_response, {complete = 0 :: integer(),
                           incomplete = 0 :: integer(),
                           interval = 0 :: second(),
                           peers = [] :: list(peer_address())}).

-type tracker_response() :: #tracker_response{}.
