-module(epgsql_pool_SUITE).

%% test needs connection to database
%% and database should be inited with ./testdb_schema.sql

-include("epgsql_pool.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         start_test/1, stop_test/1, equery_test/1, transaction_test/1, reconnect_test/1
        ]).


all() ->
    [start_test,
     stop_test,
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


start_test(Config) ->
    ok.


stop_test(Config) ->
    ok.

equery_test(Config) ->
    Connection = proplists:get_value(connection, Config),
    #epgsql_connection{connection_sock = Sock} = Connection,
    Res = epgsql:equery(Sock, "SELECT * FROM item"), % TODO epgsql_pool:equery, after start
    ct:pal("Res:~p", [Res]),
    ok.


transaction_test(Config) ->
    Connection = proplists:get_value(connection, Config),
    #epgsql_connection{connection_sock = Sock} = Connection,
    Res = epgsql:equery(Sock, "SELECT * FROM category"), % TODO epgsql_pool:transaction
    ct:pal("Res:~p", [Res]),
    ok.


reconnect_test(Config) ->
    ok.
