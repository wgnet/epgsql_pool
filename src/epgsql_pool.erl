-module(epgsql_pool).

-export([start/4, stop/1,
         validate_connection_params/1,
         query/2, query/3, query/4,
         transaction/2,
         get_settings/0, set_settings/1
        ]).

-include("epgsql_pool.hrl").

-type(pool_name() :: binary() | string() | atom()).
-export_type([pool_name/0]).


%% Module API

-spec start(pool_name(), integer(), integer(), map() | #epgsql_connection_params{}) -> {ok, pid()} | {error, term()}.
start(PoolName, InitCount, MaxCount, ConnectionParams) when is_map(ConnectionParams) ->
    Params2 = #epgsql_connection_params{
                 host = maps:get(host, ConnectionParams),
                 port = maps:get(port, ConnectionParams),
                 username = maps:get(username, ConnectionParams),
                 password = maps:get(password, ConnectionParams),
                 database = maps:get(database, ConnectionParams)
                },
    start(PoolName, InitCount, MaxCount, Params2);

start(PoolName0, InitCount, MaxCount, #epgsql_connection_params{} = ConnectionParams) ->
    epgsql_pool_settings:set_connection_params(my_pool, ConnectionParams),
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    case epgsql_pool_settings:get_connection_params(PoolName) of
        {ok, _} -> PoolConfig = [{name, PoolName},
                                 {init_count, InitCount},
                                 {max_count, MaxCount},
                                 {start_mfa, {epgsql_pool_worker, start_link, [PoolName]}},
                                 {stop_mfa, {epgsql_pool_worker, stop, []}}
                                ],
                   pooler:new_pool(PoolConfig);
        {error, not_found} -> {error, connection_params_not_found}
    end.


-spec stop(pool_name()) -> ok | {error, term()}.
stop(PoolName) ->
    pooler:rm_pool(epgsql_pool_utils:pool_name_to_atom(PoolName)).


-spec validate_connection_params(#epgsql_connection_params{}) -> ok | {error, term()}.
validate_connection_params(#epgsql_connection_params{host = Host, port = Port, username = Username,
                                                     password = Password, database = Database}) ->
    ConnectionTimeout = epgsql_pool_settings:get(connection_timeout),
    Res = epgsql:connect(Host, Username, Password,
                         [{port, Port},
                          {database, Database},
                          {timeout, ConnectionTimeout}]),
    case Res of
        {ok, Sock} -> epgsql:close(Sock), ok;
        {error, Reason} -> {error, Reason}
    end.


-spec query(pool_name() | pid(), epgsql:sql_query()) -> epgsql:reply().
query(PoolNameOrWorker, Stmt) ->
    query(PoolNameOrWorker, Stmt, [], []).


-spec query(pool_name() | pid(), epgsql:sql_query(), [epgsql:bind_param()]) -> epgsql:reply().
query(PoolNameOrWorker, Stmt, Params) ->
    query(PoolNameOrWorker, Stmt, Params, []).


-spec query(pool_name() | pid(), epgsql:sql_query(), [epgsql:bind_param()], [proplists:option()]) -> epgsql:reply().
query(Worker, Stmt, Params, Options) when is_pid(Worker) ->
    Timeout = case proplists:get_value(timeout, Options) of
                  undefined -> epgsql_pool_settings:get(query_timeout);
                  V -> V
              end,
    try
        gen_server:call(Worker, {equery, Stmt, Params}, Timeout)
    catch
        exit:{timeout, _} ->
            gen_server:call(Worker, cancel),
            error_logger:error_msg("query timeout ~p ~p", [Stmt, Params]),
            {error, timeout}
    end;

query(PoolName0, Stmt, Params, Options) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    case get_worker(PoolName) of
        {ok, Worker} ->
            try
                query(Worker, Stmt, Params, Options)
            catch
                Err:Reason ->
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(PoolName, Worker, ok)
            end;
        {error, Reason} -> {error, Reason}
    end.


-spec transaction(pool_name(), fun()) -> epgsql:reply() | {error, term()}.
transaction(PoolName0, Fun) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    case get_worker(PoolName) of
        {ok, Worker} ->
            try
                gen_server:call(Worker, {squery, "BEGIN"}),
                Result = Fun(Worker),
                gen_server:call(Worker, {squery, "COMMIT"}),
                Result
            catch
                Err:Reason ->
                    gen_server:call(Worker, {squery, "ROLLBACK"}),
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(PoolName, Worker, ok)
            end;
        {error, Reason} -> {error, Reason}
    end.


-spec get_settings() -> map().
get_settings() ->
    lists:foldl(fun(Key, Map) ->
                        maps:put(Key, epgsql_pool_settings:get(Key), Map)
                end, maps:new(), epgsql_pool_settings:all_keys()).


-spec set_settings(map()) -> ok.
set_settings(Map) ->
    lists:foreach(fun(Key) ->
                          case maps:find(Key, Map) of
                              {ok, Value} -> epgsql_pool_settings:set(Key, Value);
                              error -> do_nothing
                          end
                  end, epgsql_pool_settings:all_keys()),
    ok.

%%% inner functions

get_worker(PoolName) ->
    Timeout = epgsql_pool_settings:get(pooler_get_worker_timeout),
    case pooler:take_member(PoolName, Timeout) of
        Worker when is_pid(Worker) -> {ok, Worker};
        error_no_members ->
            PoolStats = pooler:pool_stats(PoolName),
            error_logger:error_msg("Pool ~p overload: ~p", [PoolName, PoolStats]),
            {error, pool_overload}
    end.
