-define(DB_QUERY_TIMEOUT, 10000).
-define(DB_POOLER_GET_WORKER_TIMEOUT, 1000).
-define(DB_MAX_RECONNECT_TIMEOUT, 3000).
-define(DB_MIN_RECONNECT_TIMEOUT, 100).


-type(pool_name() :: binary() | string() | atom()).
-type(db_query() :: binary() | string()). % TODO: did driver accepts string?
-type(db_reply() :: term()). % TODO: narrow type


-record(epgsql_connection_params, {
    host                :: string() | binary(),
    port                :: non_neg_integer(),
    username            :: string() | binary(),
    password            :: string() | binary(),
    database            :: string() | binary()
}).

-record(epgsql_connection, {
    connection            :: pid(),
    params                :: #epgsql_connection_params{},
    connection_timeout    :: non_neg_integer(),
    query_timeout         :: non_neg_integer(),
    reconnect_attempt = 0 :: non_neg_integer(),
    reconnect_timeout = 0 :: non_neg_integer()
}).
