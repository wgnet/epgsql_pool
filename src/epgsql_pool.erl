-module(epgsql_pool).

-export([
         start/3, stop/1,
         equery/2, equery/3,
         transaction/2
        ]).

-include("epgsql_pool.hrl").

-import(epgsql_pool_utils, [pool_name_to_atom/1]).


%% Module API

-spec start(pool_name(), integer(), integer()) -> ok. % TODO what returns from pooler:new_pool?
start(PoolName0, InitCount, MaxCount) ->
    PoolName = pool_name_to_atom(PoolName0),
    PoolConfig = [
        {name, PoolName},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {epgsql_pool_worker, start_link, [PoolName]}}
    ],
    pooler:new_pool(PoolConfig).


-spec stop(pool_name()) -> ok. % TODO what returns from pooler:stop?
stop(PoolName) ->
    pooler:rm_pool(pool_name_to_atom(PoolName)).


-spec equery(pool_name(), db_query()) -> db_reply().
equery(PoolName, Stmt) ->
    equery(PoolName, Stmt, []).


%% Example
%% epgsql_pool:equery("my_db_pool", "SELECT NOW() as now", []).
-spec equery(pool_name(), db_query(), list()) -> db_reply().
equery(PoolName, Stmt, Params) ->
    transaction(PoolName,
                fun(Worker) ->
                        equery_with_worker(Worker, Stmt, Params)
                end).


-spec transaction(pool_name(), fun()) -> db_reply() | {error, term()}.
transaction(PoolName0, Fun) ->
    PoolName = pool_name_to_atom(PoolName0),
    case pooler:take_member(PoolName, ?DB_POOLER_GET_WORKER_TIMEOUT) of
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


%% Inner functions

-spec equery_with_worker(pid(), db_query(), list()) -> db_reply().
equery_with_worker(Worker, Stmt, Params) ->
    gen_server:call(Worker, {equery, Stmt, Params}, ?DB_QUERY_TIMEOUT).
