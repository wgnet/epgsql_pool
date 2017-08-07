-module(epgsql_test).

-export([test_run/0, on_connect/1, on_disconnect/1]).

-spec test_run() -> ok.
test_run() ->
    application:ensure_all_started(epgsql_pool),
    application:set_env(epgsql_pool, on_connect_callback, {?MODULE, on_connect}),
    application:set_env(epgsql_pool, on_disconnect_callback, {?MODULE, on_disconnect}),

    Params = #{host => "localhost",
        port => 5432,
        username => "test",
        password => "test",
        database => "testdb"},
    {ok, _} = epgsql_pool:start(my_pool, 2, 2, Params),

    Qs = [
        "create table category (id int, name varchar)",
        "insert into category values (1, 'some')",
        "select * from category"
    ],

    lists:foreach(
        fun(Q) ->
            Res = epgsql_pool:query(my_pool, Q),
            error_logger:info_msg("Q:~p~nRes:~p~n", [Q, Res])
        end,
        Qs),
    ok.


-spec on_connect(term()) -> ok.
on_connect(PoolName) ->
    error_logger:info_msg("On Connect ~p", [PoolName]),
    ok.


-spec on_disconnect(term()) -> ok.
on_disconnect(PoolName) ->
    error_logger:info_msg("On Disconnect ~p", [PoolName]),
    ok.


