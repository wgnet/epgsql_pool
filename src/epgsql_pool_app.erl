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
    Params = #epgsql_connection_params{host = "localhost",
                                       port = 5432,
                                       username = "test",
                                       password = "test",
                                       database = "testdb"},
    epgsql_pool_settings:set_connection_params(my_pool, Params),
    {ok, _} = epgsql_pool:start(my_pool, 1, 2),

    ok.
