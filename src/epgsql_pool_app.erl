-module(epgsql_pool_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    lists:foreach(fun start_pool/1, wgconfig:list_sections("epgsql_pool")),

    folsom_metrics:new_spiral(<<"db_connection_errors">>),
    folsom_metrics:new_spiral(<<"db_common_errors">>),
    folsom_metrics:new_spiral(<<"db_request_timeouts">>),
    {ok, self()}.

stop(_State) ->
    lists:foreach(fun stop_pool/1, wgconfig:list_sections("epgsql_pool")),
    ok.

%% Internal functions

-include("epgsql_pool.hrl").

start_pool(PoolName) ->
    InitCount = wgconfig:get_int(PoolName, init_count),
    MaxCount = wgconfig:get_int(PoolName, max_count),

    PoolConfig = [
        {name, erlang:binary_to_atom(PoolName, utf8)},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {epgsql_pool_worker, start_link, [PoolName]}}
    ],
    pooler:new_pool(PoolConfig).

stop_pool(PoolName) ->
    APoolName = erlang:binary_to_atom(PoolName, utf8),
    pooler:rm_pool(APoolName).
