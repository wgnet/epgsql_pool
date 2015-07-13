-module(epgsql_pool).

-export([
    transaction/1,
    equery/2
]).

-include("epgsql_pool.hrl").

-define(TIMEOUT, 1000).

equery(Stmt, Params) ->
    % Example
    % epgsql_pool:equery("SELECT NOW() as now", []).
    transaction(
        fun(Worker) ->
            equery({worker, Worker}, Stmt, Params)
        end).

equery({worker, Worker}, Stmt, Params) ->
    gen_server:call(Worker, {equery, Stmt, Params}, infinity).

transaction(Fun) ->
    % TODO: logging with time of execution
    case pooler:take_member(?POOL_NAME, ?TIMEOUT) of
        Worker when is_pid(Worker) ->
            W = {worker, Worker},
            try
                equery(W, "BEGIN", []),
                Result = Fun(Worker),
                equery(W, "COMMIT", []),
                Result
            catch
                Err:Reason ->
                    equery(W, "ROLLBACK", []),
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(?POOL_NAME, Worker, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(?POOL_NAME),
            lager:warning("Pool overload: ~p", [PoolStats]),
            {error, no_members}
    end.