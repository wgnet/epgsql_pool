-module(epgsql_pool_SUITE).

%% test needs connection to database
%% and database should be inited with ./testdb_schema.sql

-include("epgsql_pool.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([all/0,
         init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         query_test/1, squery_test/1, transaction_test/1, reconnect_test/1, timeout_test/1,
         validate_connection_params_test/1
        ]).

-define(SELECT_ITEMS_QUERY, "SELECT id, category_id, title, num FROM item ORDER by id ASC").

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() ->
    [query_test,
     squery_test,
     transaction_test,
     reconnect_test,
     timeout_test,
     validate_connection_params_test
    ].


-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    application:ensure_all_started(epgsql_pool),
    Config.


-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    application:stop(epgsql_pool),
    Config.


-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    Params = #epgsql_connection_params{host = "localhost", port = 5432, username = "test", password = "test", database = "testdb"},
    {ok, _} = epgsql_pool:start(my_pool, 5, 10, Params),
    epgsql_pool:query(my_pool, "TRUNCATE TABLE item"),
    epgsql_pool:query(my_pool, "TRUNCATE TABLE category CASCADE"),
    Config.


-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_TestCase, Config) ->
    ok = epgsql_pool:stop(my_pool),
    Config.


-spec query_test(config()) -> ok.
query_test(_Config) ->
    {ok, 3, _, Ids} = epgsql_pool:query(my_pool,
                                        "INSERT INTO category (title) "
                                        "VALUES ('cat 1'), ('cat 2'), ('cat 3') "
                                        "RETURNING id"),
    WaitForRows = lists:map(fun({{Id}, Title}) -> {Id, Title} end,
                            lists:zip(Ids, [<<"cat 1">>, <<"cat 2">>, <<"cat 3">>])),
    {ok, _, Rows} = epgsql_pool:query(my_pool, "SELECT id, title FROM category ORDER by id ASC"),
    ct:log("Rows ~p", [Rows]),
    ?assertEqual(WaitForRows, Rows),

    [{Id1}, {Id2} | _] = Ids,
    {ok, _, Rows2} = epgsql_pool:query(my_pool, "SELECT id, title FROM category WHERE id = $1 OR id = $2 ORDER BY id ASC", [Id1, Id2]),
    ct:log("Rows2 ~p", [Rows2]),
    ?assertEqual([{Id1, <<"cat 1">>}, {Id2, <<"cat 2">>}], Rows2),

    {error, Error} = epgsql_pool:query(my_pool, "SELECT id, title FROM some_table"),
    ct:log("Error:~p", [Error]),
    ?assertMatch(#error{severity = error, message = <<"relation \"some_table\" does not exist">>}, Error),
    ok.


-spec squery_test(config()) -> ok.
squery_test(Config) ->
    {ok, 3, _, Ids} = epgsql_pool:squery(my_pool,
                                         "INSERT INTO category (title) "
                                         "VALUES ('cat 1'), ('cat 2'), ('cat 3') "
                                         "RETURNING id"),
    WaitForRows = lists:map(fun({{Id}, Title}) -> {Id, Title} end,
                            lists:zip(Ids, [<<"cat 1">>, <<"cat 2">>, <<"cat 3">>])),
    {ok, _, Rows} = epgsql_pool:squery(my_pool, "SELECT id, title FROM category ORDER by id ASC"),
    ct:log("Rows ~p", [Rows]),
    ?assertEqual(WaitForRows, Rows),

    ?assertMatch([{ok, _, [{<<"1">>}]}, {ok, _, [{<<"2">>}]}],
                 epgsql_pool:squery(my_pool, "SELECT 1; SELECT 2;")),

    ?assertMatch({error, #error{severity = error, message = <<"relation \"some_table\" does not exist">>}},
                 epgsql_pool:squery(my_pool, "SELECT * FROM some_table")),
    Config.


-spec transaction_test(config()) -> ok.
transaction_test(_Config) ->
    {FirstCatId, CatIds2, ItemIds2} =
        epgsql_pool:transaction(my_pool,
                                fun(Worker) ->
                                        ct:log("worker:~p", [Worker]),
                                        {ok, 3, _, CatIds0} =
                                            epgsql_pool:query(Worker,
                                                              "INSERT INTO category (title) "
                                                              "VALUES ('cat 4'), ('cat 5'), ('cat 6') "
                                                              "RETURNING id"),
                                        CatIds1 = lists:map(fun({Cid}) -> Cid end, CatIds0),
                                        CatId = hd(CatIds1),
                                        {ok, 2, _, ItemIds0} =
                                            epgsql_pool:query(Worker,
                                                              "INSERT INTO item (category_id, title, num) "
                                                              "VALUES ($1, 'item 1', 5), ($1, 'item 2', 7) "
                                                              "RETURNING id", [CatId]),
                                        ItemIds1 = lists:map(fun({Iid}) -> Iid end, ItemIds0),
                                        {CatId, CatIds1, ItemIds1}
                                end),
    WaitForCats = lists:zip(CatIds2, [<<"cat 4">>, <<"cat 5">>, <<"cat 6">>]),
    {ok, _, CatRows} = epgsql_pool:query(my_pool, "SELECT id, title FROM category ORDER by id ASC"),
    ct:log("CatRows ~p", [CatRows]),
    ?assertEqual(WaitForCats, CatRows),

    WaitForItems = lists:map(fun({ItemId, {Title, Num}}) -> {ItemId, FirstCatId, Title, Num} end,
                             lists:zip(ItemIds2, [{<<"item 1">>, 5}, {<<"item 2">>, 7}])),
    {ok, _, ItemRows} = epgsql_pool:query(my_pool, ?SELECT_ITEMS_QUERY),
    ct:log("ItemRows ~p", [ItemRows]),
    ?assertEqual(WaitForItems, ItemRows),

    try
        epgsql_pool:transaction(my_pool,
                                fun(Worker) ->
                                        ct:log("worker:~p", [Worker]),
                                        {ok, 2} =
                                            epgsql_pool:query(Worker,
                                                              "INSERT INTO item (category_id, title, num) "
                                                              "VALUES ($1, 'item 3', 55), ($1, 'item 4', 77) ",
                                                              [FirstCatId]),
                                        {ok, _, ItemRows2} = epgsql_pool:query(Worker, ?SELECT_ITEMS_QUERY),
                                        ct:log("ItemRows2 ~p", [ItemRows2]),
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
    {ok, _, ItemRows} = epgsql_pool:query(my_pool, ?SELECT_ITEMS_QUERY),
    ok.


-spec reconnect_test(config()) -> ok.
reconnect_test(_Config) ->
    Worker = pooler:take_member(my_pool, 1000),
    [state, my_pool, #epgsql_connection{sock = Sock1} | _]= tuple_to_list(sys:get_state(Worker)),
    ct:log("Worker: ~p, sock: ~p", [Worker, Sock1]),

    R1 = epgsql_pool:query(Worker, ?SELECT_ITEMS_QUERY),
    ct:log("first query ~p", [R1]),
    {ok, _, []} = R1,

    ct:log("~p close_connection", [Sock1]),
    exit(Sock1, close_connection),

    R2 = epgsql_pool:query(Worker, "select * from item"),
    ct:log("second query goes immediatelly ~p", [R2]),
    {error, no_connection} = R2,

    timer:sleep(50),

    R3 = epgsql_pool:query(Worker, "select * from item"),
    ct:log("third query goes after 50 ms ~p", [R3]),
    {error, no_connection} = R3,

    timer:sleep(150),

    R4 = epgsql_pool:query(Worker, "select * from item"),
    ct:log("fouth query goes after 200 ms ~p", [R4]),
    {ok, _, []} = R4,

    [state, my_pool, #epgsql_connection{sock = Sock2} | _]= tuple_to_list(sys:get_state(Worker)),
    ct:log("Worker: ~p, sock: ~p", [Worker, Sock2]),

    ?assertNotEqual(Sock1, Sock2),
    ok.


-spec timeout_test(config()) -> ok.
timeout_test(_Config) ->
    Res1 = epgsql_pool:query(my_pool, "SELECT pg_sleep(1)", [], [{timeout, 2000}]),
    ct:log("Res1:~p", [Res1]),
    ?assertMatch({ok, _, _}, Res1),

    Res2 = epgsql_pool:query(my_pool, "SELECT pg_sleep(1)", [], [{timeout, 500}]),
    ct:log("Res2:~p", [Res2]),
    ?assertEqual({error, timeout}, Res2),

    Worker = pooler:take_member(my_pool, 1000),

    Res3 = epgsql_pool:query(Worker, "SELECT pg_sleep(100)", [], [{timeout, 500}]),
    ct:log("Res3:~p", [Res3]),
    ?assertEqual({error, timeout}, Res3),

    %% check worker and connection able to perform query
    Res4 = epgsql_pool:query(Worker, "SELECT * FROM item", [], [{timeout, 500}]),
    ct:log("Res4:~p", [Res4]),
    ?assertMatch({ok, _, _}, Res4),

    ok.


-spec validate_connection_params_test(config()) -> ok.
validate_connection_params_test(_Config) ->
    Params1 = #epgsql_connection_params{host = "localhost", port = 5432,
                                        username = "test", password = "test", database = "testdb"},
    Res1 = epgsql_pool:validate_connection_params(Params1),
    ct:log("Res1: ~p", [Res1]),
    ?assertEqual(ok, Res1),

    Params2 = #epgsql_connection_params{host = "localhost", port = 5432,
                                        username = "test", password = "some", database = "testdb"},
    Res2 = epgsql_pool:validate_connection_params(Params2),
    ct:log("Res2: ~p", [Res2]),
    ?assertEqual({error,invalid_password}, Res2),

    Params3 = #epgsql_connection_params{host = "localhost", port = 5432,
                                        username = "test", password = "test", database = "some"},
    {error, Res3} = epgsql_pool:validate_connection_params(Params3),
    ct:log("Res3: ~p", [Res3]),
    ?assertMatch(#error{
                    severity = fatal,
                    code = <<"3D000">>,
                    codename = invalid_catalog_name,
                    message = <<"database \"some\" does not exist">>
                   }, Res3),

    ok.
