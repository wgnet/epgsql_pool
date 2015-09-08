-module(epgsql_pool).

-export([start/3, stop/1,
         equery/2, equery/3, equery/4,
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


-spec equery(pool_name() | pid(), epgsql:sql_query()) -> epgsql:reply().
equery(PoolNameOrWorker, Stmt) ->
    equery(PoolNameOrWorker, Stmt, []).


%% Example
%% epgsql_pool:equery("my_db_pool", "SELECT NOW() as now", []).
-spec equery(pool_name() | pid(), epgsql:sql_query(), [epgsql:bind_param()]) -> epgsql:reply().
equery(Worker, Stmt, Params) when is_pid(Worker) ->
    Timeout = epgsql_pool_settings:get(query_timeout),
    % TODO process timeout,
    % try-catch
    % send cancel
    % log error
    % reply to client with error
    % reconnect
    % return to pool
    gen_server:call(Worker, {equery, Stmt, Params}, Timeout);

equery(PoolName, Stmt, Params) ->
    transaction(PoolName,
                fun(Worker) ->
                        equery(Worker, Stmt, Params)
                end).


-spec equery(pool_name() | pid(), epgsql:sql_query(), [epgsql:bind_param()], fun()) -> epgsql:reply().
equery(PoolName, Stmt, Params, ErrorHandler) ->
    Res = equery(PoolName, Stmt, Params),
    case Res of
        {error, Error} -> ErrorHandler(PoolName, Stmt, Params, Error);
        _ -> Res
    end.


-spec transaction(pool_name(), fun()) -> epgsql:reply() | {error, term()}.
transaction(PoolName0, Fun) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    Timeout = epgsql_pool_settings:get(pooler_get_worker_timeout),
    case pooler:take_member(PoolName, Timeout) of
        Worker when is_pid(Worker) ->
            try
                equery(Worker, "BEGIN", []),
                Result = Fun(Worker),
                equery(Worker, "COMMIT", []),
                Result
            catch
                Err:Reason ->
                    equery(Worker, "ROLLBACK", []),
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(PoolName, Worker, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(PoolName),
            error_logger:error_msg("Pool ~p overload: ~p", [PoolName, PoolStats]),
            {error, pool_overload}
    end.
