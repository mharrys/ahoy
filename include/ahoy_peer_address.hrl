-define(PEER_ID, "~~~~~~~~AHOY~~~~~~~~").

-type ip_address() :: inet:ip_address().
-type port_number() :: inet:port_number().

%% Describes a peer address
-record(peer_address, {ip :: ip_address(),
                       port :: port_number()}).

-type peer_address() :: #peer_address{}.
