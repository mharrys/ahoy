-define(PEER_ID, "~~~~~~~~AHOY~~~~~~~~").

-type ip_address() :: inet:ip_address().
-type port_number() :: inet:port_number().

%% Describes a peer
-record(peer, {ip :: ip_address(),
               port :: port_number()}).
