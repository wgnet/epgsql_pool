-module(epgsql_pool_SUITE).

%% test needs connection to database
%% and database should be inited with ./testdb_schema.sql

-include("epgsql_pool.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([all/0,
         init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         equery_test/1, transaction_test/1, reconnect_test/1, error_handler_test/1
        ]).

-define(SELECT_ITEMS_QUERY, "SELECT id, category_id, title, num FROM item ORDER by id ASC").


all() ->
    [equery_test,
     transaction_test,
     reconnect_test,
     error_handler_test
    ].


init_per_suite(Config) ->
    application:ensure_all_started(epgsql_pool),
    Config.


end_per_suite(Config) ->
    application:stop(epgsql_pool),
    Config.


init_per_testcase(_, Config) ->
    Params = #epgsql_connection_params{host = "localhost", port = 5432, username = "test", password = "test", database = "testdb"},
    epgsql_pool_settings:set_connection_params(my_pool, Params),
    {ok, _} = epgsql_pool:start(my_pool, 5, 10),
    epgsql_pool:equery(my_pool, "TRUNCATE TABLE item"),
    epgsql_pool:equery(my_pool, "TRUNCATE TABLE category CASCADE"),
    Config.


end_per_testcase(_, Config) ->
    ok = epgsql_pool:stop(my_pool),
    Config.


equery_test(Config) ->
    {ok, 3, _, Ids} = epgsql_pool:equery(my_pool,
                                         "INSERT INTO category (title) "
                                         "VALUES ('cat 1'), ('cat 2'), ('cat 3') "
                                         "RETURNING id"),
    WaitForRows = lists:map(fun({{Id}, Title}) -> {Id, Title} end,
                            lists:zip(Ids, [<<"cat 1">>, <<"cat 2">>, <<"cat 3">>])),
    {ok, _, Rows} = epgsql_pool:equery(my_pool, "SELECT id, title FROM category ORDER by id ASC"),
    ct:pal("Rows ~p", [Rows]),
    ?assertEqual(WaitForRows, Rows),
    ok.


transaction_test(Config) ->
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
    ok.


reconnect_test(Config) ->
    Worker = pooler:take_member(my_pool, 1000) ,
    [state, my_pool, #epgsql_connection{sock = Sock1} | _]= tuple_to_list(sys:get_state(Worker)),
    ct:pal("Worker: ~p, sock: ~p", [Worker, Sock1]),

    R1 = epgsql_pool:equery(Worker, ?SELECT_ITEMS_QUERY),
    ct:pal("first query ~p", [R1]),
    {ok, _, []} = R1,

    ct:pal("~p close_connection", [Sock1]),
    exit(Sock1, close_connection),

    R2 = epgsql_pool:equery(Worker, "select * from item"),
    ct:pal("second query goes immediatelly ~p", [R2]),
    {error, reconnecting} = R2,

    timer:sleep(50),

    R3 = epgsql_pool:equery(Worker, "select * from item"),
    ct:pal("third query goes after 50 ms ~p", [R3]),
    {error, reconnecting} = R3,

    timer:sleep(150),

    R4 = epgsql_pool:equery(Worker, "select * from item"),
    ct:pal("fouth query goes after 200 ms ~p", [R4]),
    {ok, _, []} = R4,

    [state, my_pool, #epgsql_connection{sock = Sock2} | _]= tuple_to_list(sys:get_state(Worker)),
    ct:pal("Worker: ~p, sock: ~p", [Worker, Sock2]),

    ?assertNotEqual(Sock1, Sock2),
    ok.


error_handler_test(Config) ->
    {error, Error} = epgsql_pool:equery(my_pool, "SELECT id, title FROM some_table"),
    ct:pal("Error:~p", [Error]),
    ?assertMatch(#error{severity = error, message = <<"relation \"some_table\" does not exist">>}, Error),

    Query2 = "SELECT some_field FROM item WHERE id = $1",
    ErrorMessage2 = <<"column \"some_field\" does not exist">>,
    ErrorHandler = fun(_Worker, Stmt, Params, Error2) ->
                           ct:pal("ErrorHandler: ~p", [Error2]),
                           ?assertEqual(Query2, Stmt),
                           ?assertEqual([1], Params),
                           ?assertMatch(#error{severity = error, message = ErrorMessage2}, Error2),
                           {db_error, Error2#error.message}
                   end,
    Res = epgsql_pool:equery(my_pool, Query2, [1], [{error_handler, ErrorHandler}]),
    ct:pal("Res:~p", [Res]),
    ?assertEqual({db_error, ErrorMessage2}, Res),

    ok.
