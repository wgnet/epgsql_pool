-module(epgsql_pool).

-export([
    equery/3,
    transaction/2
]).

-include("epgsql_pool.hrl").

% TODO: move parameter into config
-define(TIMEOUT, 1000).

equery({worker, _PoolName, Worker}, Stmt, Params) ->
    % TODO: infinity - looks as dangerous
    gen_server:call(Worker, {equery, Stmt, Params}, infinity);
equery(PoolName, Stmt, Params) ->
    % Example
    % epgsql_pool:equery(<<"default">>, "SELECT NOW() as now", []).
    transaction(
        PoolName,
        fun(Worker) ->
            equery(Worker, Stmt, Params)
        end).

transaction(PoolName, Fun) ->
    FullPoolName = list_to_atom("epgsql_pool." ++ binary_to_list(PoolName)),
    % TODO: logging with time of execution
    case pooler:take_member(FullPoolName, ?TIMEOUT) of
        Pid when is_pid(Pid) ->
            Worker = {worker, FullPoolName, Pid},
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
                pooler:return_member(FullPoolName, Pid, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(FullPoolName),
            lager:warning("Pool ~p overload: ~p", [FullPoolName, PoolStats]),
            {error, no_members}
    end.