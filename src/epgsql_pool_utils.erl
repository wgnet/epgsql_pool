-module(epgsql_pool_utils).

-export([open_connection/2,
         close_connection/1,
         reconnect/1,
         pool_name_to_atom/1
        ]).

-include("epgsql_pool.hrl").


-spec open_connection(atom(), #epgsql_connection{} | undefined) -> {ok, #epgsql_connection{}} | {error, term(), #epgsql_connection{}}.
open_connection(PoolName, undefined) ->
    open_connection(PoolName, #epgsql_connection{});
open_connection(PoolName, Connection0) ->
    {ok,Params} = epgsql_pool_settings:get_connection_params(PoolName),
    #epgsql_connection_params{host = Host, port = Port, username = Username,
                              password = Password, database = Database} = Params,
    ConnectionTimeout = epgsql_pool_settings:get(connection_timeout),
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
    MaxTimeout = epgsql_pool_settings:get(max_reconnect_timeout),
    MinTimeout = epgsql_pool_settings:get(min_reconnect_timeout),
    Timeout = exponential_backoff(Attempt, 10, MinTimeout, MaxTimeout),
    error_logger:warning_msg("epgsql_pool reconnect after ~p attempt ~p", [Timeout, Attempt]),
    erlang:send_after(Timeout, self(), open_connection),
    Connection#epgsql_connection{reconnect_attempt = Attempt + 1}.


-spec exponential_backoff(integer(), integer(), integer(), integer()) -> integer().
exponential_backoff(Attempt, MaxAttempt, _BaseTimeout, MaxTimeout) when Attempt >= MaxAttempt ->
    Half = MaxTimeout div 2,
    Half + random:uniform(Half);
exponential_backoff(Attempt, _MaxAttempt, BaseTimeout, MaxTimeout) ->
    Timeout = min(erlang:round(math:pow(2, Attempt) * BaseTimeout), MaxTimeout),
    Half = Timeout div 2,
    Half + random:uniform(Half).


-spec pool_name_to_atom(epgsql_pool:pool_name()) -> atom().
pool_name_to_atom(PoolName) when is_binary(PoolName) ->
    pool_name_to_atom(erlang:binary_to_atom(PoolName, utf8));
pool_name_to_atom(PoolName) when is_list(PoolName) ->
    pool_name_to_atom(list_to_atom(PoolName));
pool_name_to_atom(PoolName) -> PoolName.
