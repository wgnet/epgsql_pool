-module(epgsql_pool_app).

-behaviour(application).

-export([start/2, stop/1, test_run/0]).

-include("epgsql_pool.hrl").


-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    epgsql_pool_sup:start_link().


-spec stop(term()) -> ok.
stop(_State) ->
    ok.


-spec test_run() -> ok.
test_run() ->
    application:ensure_all_started(epgsql_pool),

    Params = #{host => "localhost",
               port => 5432,
               username => "test",
               password => "test",
               database => "testdb"},
    {ok, _} = epgsql_pool:start(my_pool, 1, 1, Params),

    Res1 = epgsql_pool:query(my_pool, "select * from category"),
    error_logger:info_msg("Res1: ~p", [Res1]),

    Res2 = epgsql_pool:query(my_pool, "select * from category where id = $1", [1], [{timeout, 200}]),
    error_logger:info_msg("Res2: ~p", [Res2]),

    Res3 = epgsql_pool:query(my_pool, "select pg_sleep(100) as A", [], [{timeout, 5000}]),
    error_logger:info_msg("Res3: ~p", [Res3]),

    Res4 = epgsql_pool:query(my_pool, "select pg_sleep(100) as B", [], [{timeout, 5000}]),
    error_logger:info_msg("Res4: ~p", [Res4]),

    Res5 = epgsql_pool:query(my_pool, "select 1 as C"),
    error_logger:info_msg("Res5: ~p", [Res5]),
    ok.
