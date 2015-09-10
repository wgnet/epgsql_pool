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
    {ok, _} = epgsql_pool:start(my_pool, 1, 2, Params),
    Res1 = epgsql_pool:query(my_pool, "select * from category"),
    error_logger:info_msg("~p", [Res1]),
    Res2 = epgsql_pool:query(my_pool, "select * from category where id = $1", [1], [{timeout, 200}]),
    error_logger:info_msg("~p", [Res2]),
    ok.
