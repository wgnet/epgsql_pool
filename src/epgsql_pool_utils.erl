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
    Params = epgsql_pool_settings:get_connection_params(PoolName),
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


-spec close_connection(#epgsql_connection{}) -> #epgsql_connection{}.
close_connection(#epgsql_connection{sock = undefined} = Connection) -> Connection;
close_connection(#epgsql_connection{sock = Sock} = Connection) ->
    epgsql:close(Sock),
    Connection#epgsql_connection{sock = undefined}.


-spec reconnect(#epgsql_connection{}) -> #epgsql_connection{}.
reconnect(#epgsql_connection{reconnect_attempt = Attempt,
                             reconnect_timeout = Timeout0} = Connection) ->
    MaxReconnectTimeout = epgsql_pool_settings:get(max_reconnect_timeout),
    MinReconnectTimeout = epgsql_pool_settings:get(min_reconnect_timeout),
    Timeout = if
                  Timeout0 > MaxReconnectTimeout -> Timeout0;
                  true -> exponential_backoff(Attempt, MinReconnectTimeout)
              end,
    reconnect_after(Attempt, MinReconnectTimeout, Timeout),
    Connection#epgsql_connection{reconnect_attempt = Attempt + 1, reconnect_timeout = Timeout}.


-spec reconnect_after(integer(), integer(), integer()) -> ok.
reconnect_after(Attempt, TMin, TMax) ->
    Delay = max(random:uniform(TMax), TMin),
    error_logger:warning_msg("epgsql_pool reconnect after ~p attempt ~p", [Delay, Attempt]),
    erlang:send_after(Delay, self(), open_connection),
    ok.


-spec exponential_backoff(integer(), integer()) -> integer().
exponential_backoff(N, T) ->
    erlang:round(math:pow(2, N)) * T.


-spec pool_name_to_atom(epgsql_pool:pool_name()) -> atom().
pool_name_to_atom(PoolName) when is_binary(PoolName) ->
    pool_name_to_atom(erlang:binary_to_atom(PoolName, utf8));
pool_name_to_atom(PoolName) when is_list(PoolName) ->
    pool_name_to_atom(list_to_atom(PoolName));
pool_name_to_atom(PoolName) -> PoolName.
