-module(epgsql_pool_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    start_pool(),
    {ok, self()}.

stop(_State) ->
    stop_pool(),
    ok.

-include("epgsql_pool.hrl").

start_pool() ->
    InitCount = wgconfig:get_int(?POOL_NAME, init_count),
    MaxCount = wgconfig:get_int(?POOL_NAME, max_count),

    % Connection parameters
    Hots = wgconfig:get_string(?POOL_NAME, host),
    Port = wgconfig:get_int(?POOL_NAME, port),
    Username = wgconfig:get_string(?POOL_NAME, username),
    Password = wgconfig:get_string(?POOL_NAME, password),
    Database = wgconfig:get_string(?POOL_NAME, database),
    ConnectionTimeout = wgconfig:get_int(?POOL_NAME, connection_timeout),
    QueryTimeout = wgconfig:get_int(?POOL_NAME, query_timeout),

    Params = #epgsql_params{
        host=Hots, port=Port,
        username=Username, password=Password,
        database=Database,
        connection_timeout=ConnectionTimeout,
        query_timeout=QueryTimeout
    },

    PoolConfig = [
        {name, ?POOL_NAME},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {epgsql_pool_worker, start_link, [Params]}}
    ],
    pooler:new_pool(PoolConfig).

stop_pool() ->
    pooler:rm_pool(?POOL_NAME).