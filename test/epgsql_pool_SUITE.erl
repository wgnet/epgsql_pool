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

-define(SELECT_ITEMS_QUERY, "SELECT id, category_id, title, num FROM item ORDER by id ASC").


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
    Connection = proplists:get_value(connection, Config),
    epgsql_pool_settings:set_connection_params(my_pool, Connection#epgsql_connection.params),
    {ok, _} = epgsql_pool:start(my_pool, 5, 10),

    {FirstCatId, CatIds2, ItemIds2} =
        epgsql_pool:transaction(my_pool,
                                fun(Worker) ->
                                        ct:pal("worker:~p", [Worker]),
                                        {ok, 3, _, CatIds0} =
                                            epgsql_pool:equery(Worker,
                                                               "INSERT INTO category (title) "
                                                               "VALUES ('cat 4'), ('cat 5'), ('cat 6') "
                                                               "RETURNING id"),
                                        CatIds1 = lists:map(fun({Cid}) -> Cid end, CatIds0),
                                        CatId = hd(CatIds1),
                                        {ok, 2, _, ItemIds0} =
                                            epgsql_pool:equery(Worker,
                                                               "INSERT INTO item (category_id, title, num) "
                                                               "VALUES ($1, 'item 1', 5), ($1, 'item 2', 7) "
                                                               "RETURNING id", [CatId]),
                                        ItemIds1 = lists:map(fun({Iid}) -> Iid end, ItemIds0),
                                        {CatId, CatIds1, ItemIds1}
                                end),
    WaitForCats = lists:zip(CatIds2, [<<"cat 4">>, <<"cat 5">>, <<"cat 6">>]),
    {ok, _, CatRows} = epgsql_pool:equery(my_pool, "SELECT id, title FROM category ORDER by id ASC"),
    ct:pal("CatRows ~p", [CatRows]),
    ?assertEqual(WaitForCats, CatRows),

    WaitForItems = lists:map(fun({ItemId, {Title, Num}}) -> {ItemId, FirstCatId, Title, Num} end,
                             lists:zip(ItemIds2, [{<<"item 1">>, 5}, {<<"item 2">>, 7}])),
    {ok, _, ItemRows} = epgsql_pool:equery(my_pool, ?SELECT_ITEMS_QUERY),
    ct:pal("ItemRows ~p", [ItemRows]),
    ?assertEqual(WaitForItems, ItemRows),

    try
        epgsql_pool:transaction(my_pool,
                                fun(Worker) ->
                                        ct:pal("worker:~p", [Worker]),
                                        {ok, 2} =
                                            epgsql_pool:equery(Worker,
                                                               "INSERT INTO item (category_id, title, num) "
                                                               "VALUES ($1, 'item 3', 55), ($1, 'item 4', 77) ",
                                                               [FirstCatId]),
                                        {ok, _, ItemRows2} = epgsql_pool:equery(Worker, ?SELECT_ITEMS_QUERY),
                                        ct:pal("ItemRows2 ~p", [ItemRows2]),
                                        ?assertMatch([{_, FirstCatId, <<"item 1">>, 5},
                                                      {_, FirstCatId, <<"item 2">>, 7},
                                                      {_, FirstCatId, <<"item 3">>, 55},
                                                      {_, FirstCatId, <<"item 4">>, 77}],
                                                     ItemRows2),
                                        throw(cancel_transaction)
                                end)
    catch
        throw:cancel_transaction -> ok
    end,

    %% items not changes after calcelled transaction
    {ok, _, ItemRows} = epgsql_pool:equery(my_pool, ?SELECT_ITEMS_QUERY),

    ok = epgsql_pool:stop(my_pool),
    ok.


reconnect_test(Config) ->
    Connection = proplists:get_value(connection, Config),
    epgsql_pool_settings:set_connection_params(my_pool, Connection#epgsql_connection.params),
    {ok, _} = epgsql_pool:start(my_pool, 5, 10),

    epgsql_pool:transaction(my_pool,
                            fun(Worker) ->
                                    {ok, _, []} = epgsql_pool:equery(Worker, ?SELECT_ITEMS_QUERY),
                                    exit(Connection#epgsql_connection.connection_sock, forse_close_connection),
                                    {ok, _, []} = epgsql_pool:equery(Worker, ?SELECT_ITEMS_QUERY)
                            end),

    ok = epgsql_pool:stop(my_pool),
    ok.
