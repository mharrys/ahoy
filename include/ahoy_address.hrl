-type ip_address() :: inet:ip_address().
-type port_number() :: inet:port_number().

%% IP, Port tuple i.e. socket address.
-type address() :: {ip_address(), port_number()}.
