-module(epgsql_pool_settings_tests).

-include("epgsql_pool.hrl").
-include_lib("eunit/include/eunit.hrl").


get_set_test() ->
    epgsql_pool_settings:start_link(),

    ?assertEqual(10000, epgsql_pool_settings:get(connection_timeout)),
    ?assertEqual(10000, epgsql_pool_settings:get(query_timeout)),
    ?assertEqual(10000, epgsql_pool_settings:get(pooler_get_worker_timeout)),

    epgsql_pool_settings:set(connection_timeout, 5000),
    ?assertEqual(5000, epgsql_pool_settings:get(connection_timeout)),

    epgsql_pool_settings:set(max_reconnect_timeout, 4500),
    ?assertEqual(4500, epgsql_pool_settings:get(max_reconnect_timeout)),

    ?assertThrow({settings_not_found, some_key},
                 epgsql_pool_settings:get(some_key)),

    epgsql_pool_settings:set(some_key, 42),
    ?assertEqual(42, epgsql_pool_settings:get(some_key)),

    epgsql_pool_settings ! stop,
    ok.


connection_params_test() ->
    epgsql_pool_settings:start_link(),

    ?assertThrow({connection_params_not_found, some_pool},
                 epgsql_pool_settings:get_connection_params(some_pool)),

    Params1 = #epgsql_connection_params{host = "localhost", port = 5432,
                                        username="user", password="123",
                                        database="db"},
    epgsql_pool_settings:set_connection_params(pool_1, Params1),

    Params2 = #epgsql_connection_params{host = "some.host", port = 5432,
                                        username="someuser", password="123",
                                        database="somedb"},
    epgsql_pool_settings:set_connection_params(pool_2, Params2),

    ?assertEqual(Params1, epgsql_pool_settings:get_connection_params(pool_1)),
    ?assertEqual(Params2, epgsql_pool_settings:get_connection_params(pool_2)),

    epgsql_pool_settings ! stop,
    ok.
