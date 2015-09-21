-module(epgsql_pool_utils).

-export([open_connection/2,
         close_connection/1,
         reconnect/1,
         pool_name_to_atom/1
        ]).

-include("epgsql_pool.hrl").


-spec open_connection(atom(), #epgsql_connection{} | undefined) ->
                             {ok, #epgsql_connection{}} | {error, term(), #epgsql_connection{}}.
open_connection(PoolName, undefined) ->
    open_connection(PoolName, #epgsql_connection{});
open_connection(PoolName, Connection0) ->
    {ok, Params} = application:get_env(epgsql_pool, PoolName),
    #epgsql_connection_params{host = Host, port = Port, username = Username,
                              password = Password, database = Database} = Params,
    {ok, ConnectionTimeout} = application:get_env(epgsql_pool, connection_timeout),
    Res = epgsql:connect(Host, Username, Password,
                         [{port, Port},
                          {database, Database},
                          {timeout, ConnectionTimeout}]),
    case Res of
        {ok, Sock} -> {ok, Connection0#epgsql_connection{sock = Sock, params = Params}};
        {error, Reason} -> {error, Reason, Connection0}
    end.


-spec close_connection(#epgsql_connection{} | undefined) -> #epgsql_connection{}.
close_connection(undefined) -> undefined;
close_connection(#epgsql_connection{sock = undefined} = Connection) -> Connection;
close_connection(#epgsql_connection{sock = Sock} = Connection) ->
    epgsql:close(Sock),
    Connection#epgsql_connection{sock = undefined, reconnect_attempt = 0}.


-spec reconnect(#epgsql_connection{}) -> #epgsql_connection{}.
reconnect(#epgsql_connection{reconnect_attempt = Attempt} = Connection) ->
    {ok, MaxTimeout} = application:get_env(epgsql_pool, max_reconnect_timeout),
    {ok, MinTimeout} = application:get_env(epgsql_pool, min_reconnect_timeout),
    Timeout = herd_reconnect:exp_backoff(Attempt, MinTimeout, MaxTimeout),
    error_logger:warning_msg("epgsql_pool reconnect after ~p attempt ~p", [Timeout, Attempt]),
    erlang:send_after(Timeout, self(), open_connection),
    Connection#epgsql_connection{reconnect_attempt = Attempt + 1}.


-spec pool_name_to_atom(epgsql_pool:pool_name()) -> atom().
pool_name_to_atom(PoolName) when is_binary(PoolName) ->
    pool_name_to_atom(erlang:binary_to_atom(PoolName, utf8));
pool_name_to_atom(PoolName) when is_list(PoolName) ->
    pool_name_to_atom(list_to_atom(PoolName));
pool_name_to_atom(PoolName) -> PoolName.
