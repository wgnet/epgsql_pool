-module(epgsql_pool_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    epgsql_pool_sup:start_link().


stop(_State) ->
    ok.
