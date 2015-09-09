-module(epgsql_pool).

-export([start/3, stop/1,
         validate_connection_params/1,
         set_monitoring_callback/1,
         query/2, query/3, query/4,
         transaction/2
        ]).

-include("epgsql_pool.hrl").

-type(pool_name() :: binary() | string() | atom()).
-export_type([pool_name/0]).


%% Module API

-spec start(pool_name(), integer(), integer()) -> {ok, pid()} | {error, term()}.
start(PoolName0, InitCount, MaxCount) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    PoolConfig = [
                  {name, PoolName},
                  {init_count, InitCount},
                  {max_count, MaxCount},
                  {start_mfa, {epgsql_pool_worker, start_link, [PoolName]}}
                 ],
    pooler:new_pool(PoolConfig).


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


-spec set_monitoring_callback(fun()) -> ok.
set_monitoring_callback(Callback) ->
    epgsql_pool_settings:set_monitoring_callback(Callback).


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
        TStart = os:timestamp(),
        Res = gen_server:call(Worker, {equery, Stmt, Params}, Timeout),
        Time = timer:now_diff(os:timestamp(), TStart),
        notify_db_event({db_query_time, Time}),
        case Res of
            {error, Error} -> notify_db_event({db_query_error, Stmt, Params, Error});
            _ -> do_nothing
        end,
        %% TODO monitor query time
        Res
    catch
        exit:{timeout, _} ->
            gen_server:call(Worker, cancel),
            error_logger:error_msg("query timeout ~p ~p", [Stmt, Params]),
            notify_db_event({db_query_timeout, Stmt, Params}),
            {error, timeout}
    end;

query(PoolName, Stmt, Params, Options) ->
    case get_worker(PoolName) of
        {ok, Worker} -> query(Worker, Stmt, Params, Options);
        {error, Reason} -> {error, Reason}
    end.


-spec transaction(pool_name(), fun()) -> epgsql:reply() | {error, term()}.
transaction(PoolName, Fun) ->
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


%%% inner functions

get_worker(PoolName0) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    Timeout = epgsql_pool_settings:get(pooler_get_worker_timeout),
    case pooler:take_member(PoolName, Timeout) of
        Worker when is_pid(Worker) -> {ok, Worker};
        error_no_members ->
            PoolStats = pooler:pool_stats(PoolName),
            error_logger:error_msg("Pool ~p overload: ~p", [PoolName, PoolStats]),
            {error, pool_overload}
    end.


notify_db_event(Event) ->
    case epgsql_pool_settings:get_monitoring_callback() of
        undefined -> do_nothing;
        F when is_function(F) -> F(Event)
    end.
