-module(epgsql_pool_app).

-behaviour(application).

-export([start/2, stop/1, test_run/0]).

-include("epgsql_pool.hrl").


start(_StartType, _StartArgs) ->
    epgsql_pool_sup:start_link().


stop(_State) ->
    ok.


test_run() ->
    application:ensure_all_started(epgsql_pool),
    Params = #epgsql_connection_params{host = "localhost", port = 5432, username = "test", password = "test", database = "testdb"},
    epgsql_pool_settings:set_connection_params(my_pool, Params),
    {ok, _} = epgsql_pool:start(my_pool, 5, 10),

    Worker = pooler:take_member(my_pool, 1000) ,
    {state, my_pool, #epgsql_connection{sock = Sock}} = sys:get_state(Worker),
    error_logger:info_msg("Worker: ~p, sock: ~p", [Worker, Sock]),

    {ok, _, R1} = epgsql_pool:equery(Worker, "select * from item"),
    error_logger:info_msg("~p", [R1]),

    %% exit(Sock, close_connection),
    epgsql:close(Sock),
    timer:sleep(1000),

    {ok, _, R2} = epgsql_pool:equery(Worker, "select * from item"),
    error_logger:info_msg("~p", [R2]),

    {state, my_pool, #epgsql_connection{sock = Sock2}} = sys:get_state(Worker),
    error_logger:info_msg("Worker: ~p, sock: ~p", [Worker, Sock2]),
    ok.
