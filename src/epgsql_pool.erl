-module(epgsql_pool).

-export([start/4, stop/1,
         validate_connection_params/1,
         query/2, query/3, query/4,
         squery/2, squery/3,
         transaction/2,
         get_settings/0, set_settings/1,
         get_worker/1,
         set_notice/2
        ]).

-include("epgsql_pool.hrl").
-include("otp_types.hrl").

-type(pool_name() :: binary() | string() | atom()).
-export_type([pool_name/0]).


%% Module API

-spec start(pool_name(), integer(), integer(), map() | #epgsql_connection_params{}) -> {ok, pid()}.
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
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    case lists:member(PoolName, all_keys()) of
        true -> {error, invalid_pool_name};
        false ->
            application:set_env(epgsql_pool, PoolName, ConnectionParams),
            {ok, MaxQueue} = application:get_env(epgsql_pool, pooler_max_queue),
            PoolConfig = [{name, PoolName},
                          {init_count, InitCount},
                          {max_count, MaxCount},
                          {queue_max, MaxQueue},
                          {start_mfa, {epgsql_pool_worker, start_link, [PoolName]}},
                          {stop_mfa, {epgsql_pool_worker, stop, ['$pooler_pid']}}
                         ],
            pooler:new_pool(PoolConfig)
    end.


-spec stop(pool_name()) -> ok | {error, term()}.
stop(PoolName) ->
    pooler:rm_pool(epgsql_pool_utils:pool_name_to_atom(PoolName)).


-spec validate_connection_params(map() | #epgsql_connection_params{}) -> ok | {error, term()}.
validate_connection_params(ConnectionParams) when is_map(ConnectionParams) ->
    Params2 = #epgsql_connection_params{
                 host = maps:get(host, ConnectionParams),
                 port = maps:get(port, ConnectionParams),
                 username = maps:get(username, ConnectionParams),
                 password = maps:get(password, ConnectionParams),
                 database = maps:get(database, ConnectionParams)
                },
    validate_connection_params(Params2);

validate_connection_params(#epgsql_connection_params{host = Host, port = Port, username = Username,
                                                     password = Password, database = Database}) ->
    {ok,ConnectionTimeout} = application:get_env(epgsql_pool, connection_timeout),
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
query(PoolNameOrWorker, Stmt, Params, Options) ->
    do_query(PoolNameOrWorker, {equery, Stmt, Params}, Options).


-spec squery(pool_name() | pid(), epgsql:sql_query()) -> epgsql:reply(epgsql:squery_row()) | {error, timeout}.
squery(PoolNameOrWorker, Stmt) ->
    squery(PoolNameOrWorker, Stmt, []).

-spec squery(pool_name() | pid(), epgsql:sql_query(), [proplists:option()]) ->
                    epgsql:reply(epgsql:squery_row()) | {error, timeout}.
squery(PoolNameOrWorker, Stmt, Options) ->
    do_query(PoolNameOrWorker, {squery, Stmt}, Options).


do_query(Worker, QueryTuple, Options) when is_pid(Worker) ->
    Timeout = case proplists:get_value(timeout, Options) of
                  undefined -> element(2, application:get_env(epgsql_pool, query_timeout));
                  V -> V
              end,
    try
        Sock = gen_server:call(Worker, get_sock),
        try
            gen_server:call(Worker, QueryTuple, Timeout)
        catch
            exit:{timeout, _} ->
                error_logger:error_msg("query timeout ~p", [QueryTuple]),
                epgsql_sock:cancel(Sock),
                {error, timeout}
        end
    catch
        exit:{timeout, _} ->
            error_logger:error_msg("get_sock timeout ~p", [QueryTuple]),
            {error, timeout}
    end;

do_query(PoolName0, QueryTuple, Options) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    with_worker(
      PoolName,
      fun(Worker) ->
              do_query(Worker, QueryTuple, Options)
      end).


-spec transaction(pool_name(), fun()) -> epgsql:reply() | {error, term()}.
transaction(PoolName0, Fun) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    Timeout = application:get_env(epgsql_pool, transaction_timeout, 20000),
    with_worker(
      PoolName,
      fun(Worker) ->
              try
                  gen_server:call(Worker, {squery, "BEGIN"}, Timeout),
                  Result = Fun(Worker),
                  gen_server:call(Worker, {squery, "COMMIT"}, Timeout),
                  Result
              catch
                  Err:Reason ->
                      gen_server:call(Worker, {squery, "ROLLBACK"}, Timeout),
                      erlang:raise(Err, Reason, erlang:get_stacktrace())
              end
      end).


-spec get_settings() -> map().
get_settings() ->
    lists:foldl(fun(Key, Map) ->
                        maps:put(Key, element(2, application:get_env(epgsql_pool, Key)), Map)
                end, maps:new(), all_keys()).


-spec set_settings(map()) -> ok.
set_settings(Map) ->
    lists:foreach(fun(Key) ->
                          case maps:find(Key, Map) of
                              {ok, Value} -> application:set_env(epgsql_pool, Key, Value);
                              error -> do_nothing
                          end
                  end, all_keys()),
    ok.

-spec set_notice(pid(), pid()) -> gs_call_reply().
set_notice(Worker, Pid) ->
    Timeout = application:get_env(epgsql_pool, query_timeout, 5000),
    gen_server:call(Worker, {set_async_receiver, Pid}, Timeout).

%%% inner functions

-spec get_worker(pool_name()) -> {ok, pid()} | {error, term()}.
get_worker(PoolName) ->
    {ok, Timeout} = application:get_env(epgsql_pool, pooler_get_worker_timeout),
    case pooler:take_member(PoolName, Timeout) of
        Worker when is_pid(Worker) -> {ok, Worker};
        error_no_members ->
            PoolStats = pooler:pool_stats(PoolName),
            error_logger:error_msg("Pool ~p overload: ~p", [PoolName, PoolStats]),
            {error, pool_overload}
    end.


with_worker(PoolName, Fun) ->
    case get_worker(PoolName) of
        {ok, Worker} ->
            Response =
                try
                    Fun(Worker)
                catch
                    Err:Reason ->
                        pooler:return_member(PoolName, Worker, fail),
                        erlang:raise(Err, Reason, erlang:get_stacktrace())
                end,
            pooler:return_member(PoolName, Worker, ok),
            Response;
        Err ->
            Err
    end.


-spec all_keys() -> [atom()].
all_keys() ->
    lists:filtermap(fun({_Key, #epgsql_connection_params{}}) -> false;
                       ({included_applications, _}) -> false;
                       ({Key, _Value}) -> {true, Key}
                    end,
                    application:get_all_env(epgsql_pool)).
