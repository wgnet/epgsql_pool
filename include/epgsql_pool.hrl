
-record(epgsql_connection_params, {
    host :: string() | binary(),
    port :: non_neg_integer(),
    username :: string() | binary(),
    password :: string() | binary(),
    database :: string() | binary()
}).

-record(epgsql_connection, {
    sock :: pid(),
    params :: #epgsql_connection_params{},
    reconnect_attempt = 0 :: non_neg_integer(),
    reconnect_timeout = 0 :: non_neg_integer()
}).
