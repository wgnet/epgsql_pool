-module(epgsql_pool_SUITE).

%% test needs connection to database
%% and database should be inited with ./testdb_schema.sql

-include("epgsql_pool.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         test_1/1
        ]).


all() -> [
    test_1
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
    [{connection, Connection}].


end_per_testcase(_, Config) ->
    Connection = proplists:get_value(connection, Config),
    epgsql_pool_utils:close_connection(Connection),
    Config.


test_1(Config) ->
    Connection = proplists:get_value(connection, Config),
    #epgsql_connection{connection_sock = Sock} = Connection,
    Res = epgsql:equery(Sock, "SELECT * FROM item"),
    ct:pal("Res:~p", [Res]),
    ok.
