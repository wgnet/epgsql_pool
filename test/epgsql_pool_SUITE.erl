-module(epgsql_pool_SUITE).

%% test needs connection to database
%% and database should be inited with ./testdb_schema.sql

-include("epgsql_pool.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([all/0,
         init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         start_stop_test/1, equery_test/1, transaction_test/1, reconnect_test/1
        ]).


all() ->
    [start_stop_test,
     equery_test,
     transaction_test,
     reconnect_test
    ].


init_per_suite(Config) ->
    application:ensure_all_started(epgsql_pool),
    Config.


end_per_suite(Config) ->
    application:stop(epgsql_pool),
    Config.


init_per_testcase(_, Config) ->
    Params = #epgsql_connection_params{host = "localhost", port = 5432, username = "test", password = "test", database = "testdb"},
    {ok, Connection} = epgsql_pool_utils:open_connection(Params),
    #epgsql_connection{connection_sock = Sock} = Connection,
    epgsql:equery(Sock, "TRUNCATE TABLE item"),
    epgsql:equery(Sock, "TRUNCATE TABLE category CASCADE"),
    [{connection, Connection} | proplists:delete(connection, Config)].


end_per_testcase(_, Config) ->
    Connection = proplists:get_value(connection, Config),
    Connection2 = epgsql_pool_utils:close_connection(Connection),
    #epgsql_connection{connection_sock = undefined} = Connection2,
    [{connection, Connection2} | proplists:delete(connection, Config)].


start_stop_test(Config) ->
    Params = #epgsql_connection_params{host = "localhost", port = 5432,
                                       username="test", password="test",
                                       database="testdb"},
    epgsql_pool_settings:set_connection_params(my_pool, Params),
    {ok, _} = epgsql_pool:start(my_pool, 5, 10),
    ok = epgsql_pool:stop(my_pool),
    ok.

equery_test(Config) ->
    Connection = proplists:get_value(connection, Config),
    epgsql_pool_settings:set_connection_params(my_pool, Connection#epgsql_connection.params),
    {ok, _} = epgsql_pool:start(my_pool, 5, 10),

    {ok, 3, _, Ids} = epgsql_pool:equery(my_pool,
                                         "INSERT INTO category (title) "
                                         "VALUES ('cat 1'), ('cat 2'), ('cat 3') "
                                         "RETURNING id"),
    WaitForRows = lists:map(fun({{Id}, Title}) -> {Id, Title} end,
                            lists:zip(Ids, [<<"cat 1">>, <<"cat 2">>, <<"cat 3">>])),
    {ok, _, Rows} = epgsql_pool:equery(my_pool, "SELECT id, title FROM category ORDER by id ASC"),
    ct:pal("Rows ~p", [Rows]),
    ?assertEqual(WaitForRows, Rows),

    ok = epgsql_pool:stop(my_pool),
    ok.


transaction_test(Config) ->
    throw(not_implemented),
    ok.


reconnect_test(Config) ->
    throw(not_implemented),
    ok.
