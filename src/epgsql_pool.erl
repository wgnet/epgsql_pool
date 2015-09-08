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
    equery(PoolNameOrWorker, Stmt, [], []).


-spec equery(pool_name() | pid(), epgsql:sql_query(), [epgsql:bind_param()]) -> epgsql:reply().
equery(PoolNameOrWorker, Stmt, Params) ->
    equery(PoolNameOrWorker, Stmt, Params, []).


-spec equery(pool_name() | pid(), epgsql:sql_query(), [epgsql:bind_param()], [proplists:option()]) -> epgsql:reply().
equery(Worker, Stmt, Params, Options) when is_pid(Worker) ->
    Timeout = epgsql_pool_settings:get(query_timeout),
    % TODO process timeout,
    % try-catch
    % send cancel
    % log error
    % reply to client with error
    % reconnect
    % return to pool
    Res = gen_server:call(Worker, {equery, Stmt, Params}, Timeout),
    ErrorHandler = proplists:get_value(error_handler, Options),
    case {ErrorHandler, Res} of
        {undefined, _} -> Res;
        {Fun, {error, Error}} -> Fun(Worker, Stmt, Params, Error);
        _ -> Res
    end;

equery(PoolName, Stmt, Params, Options) ->
    transaction(PoolName,
                fun(Worker) ->
                        equery(Worker, Stmt, Params, Options)
                end).


-spec transaction(pool_name(), fun()) -> epgsql:reply() | {error, term()}.
transaction(PoolName0, Fun) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    Timeout = epgsql_pool_settings:get(pooler_get_worker_timeout),
    case pooler:take_member(PoolName, Timeout) of
        Worker when is_pid(Worker) ->
            try
                equery(Worker, "BEGIN"),
                Result = Fun(Worker),
                equery(Worker, "COMMIT"),
                Result
            catch
                Err:Reason ->
                    equery(Worker, "ROLLBACK"),
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(PoolName, Worker, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(PoolName),
            error_logger:error_msg("Pool ~p overload: ~p", [PoolName, PoolStats]),
            {error, pool_overload}
    end.
