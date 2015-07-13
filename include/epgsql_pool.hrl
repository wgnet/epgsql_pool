
-record(epgsql_params, {
    host                :: string(),
    port                :: non_neg_integer(),
    username            :: string(),
    password            :: string(),
    database            :: string(),
    connection_timeout  :: non_neg_integer(),
    query_timeout       :: non_neg_integer()
}).

-define(POOL_NAME, epgsql_pool).