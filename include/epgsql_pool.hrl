-define(MAX_RECONNECT_TIMEOUT, 1000*30).
-define(MIN_RECONNECT_TIMEOUT, 100).

-record(epgsql_params, {
    host                :: string(),
    port                :: non_neg_integer(),
    username            :: string(),
    password            :: string(),
    database            :: string()
}).

-record(epgsql_connection, {
    connection            :: pid(),
    params                :: #epgsql_params{},
    connection_timeout  :: non_neg_integer(),
    query_timeout       :: non_neg_integer(),
    reconnect_attempt = 0 :: non_neg_integer(),
    reconnect_timeout = 0 :: non_neg_integer()
}).
