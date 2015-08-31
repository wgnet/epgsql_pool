-module(epgsql_pool_utils).

-export([open_connection/1,
         close_connection/1,
         reconnect/1,
         pool_name_to_atom/1
        ]).

-include_lib("epgsql/include/epgsql.hrl").
-include("epgsql_pool.hrl").


-spec open_connection(#epgsql_connection_params{}) -> #epgsql_connection{}.
open_connection(#epgsql_params{host = Host, port = Port,
                               username = Username,
                               password = Password,
                               database = Database} = ConnectionParams) ->
    Connection0 = #epgsql_connection{params = ConnectionParams, reconnect_attempt=0},
    ConnectionTimeout = epgsql_pool_settings:get(connection_timeout),
    Res = epgsql:connect(Host, Username, Password,
                         [{port, Port},
                          {database, Database},
                          {timeout, ConnectionTimeout}]),
    case Res of
        {ok, Sock} -> {ok, Connection0#epgsql_connection{connection_sock = Sock}};
        {error, Reason} -> {error, Reason, Connection0}
    end.


-spec close_connection(#epgsql_connection{}) -> #epgsql_connection{}.
close_connection(#epgsql_connection{connection_sock = Sock} = Connection) ->
    epgsql:close(Sock),
    Connection#epgsql_connection{connection_pid = undefined}.


-spec reconnect(#epgsql_connection{}) -> #epgsql_connection{}.
reconnect(#epgsql_connection{reconnect_attempt = R,
                             reconnect_timeout = T} = Connection) ->
    case T > ?DB_MAX_RECONNECT_TIMEOUT of
        true ->
            reconnect_after(?DB_MIN_RECONNECT_TIMEOUT, T),
            Connection#epgsql_connection{reconnect_attempt = R + 1};
        _ ->
            T2 = exponential_backoff(R, ?DB_MIN_RECONNECT_TIMEOUT),
            reconnect_after(?DB_MIN_RECONNECT_TIMEOUT, T2),
            Connection#epgsql_connection{reconnect_attempt = R + 1, reconnect_timeout = T2}
    end.


-spec reconnect_after(integer(), integer()) -> ok.
reconnect_after(TMin, TMax) ->
    Delay = max(random:uniform(TMax), TMin),
    erlang:send_after(Delay, self(), open_connection),
    ok.


-spec exponential_backoff(integer(), integer()) -> integer().
exponential_backoff(N, T) ->
    erlang:round(math:pow(2, N)) * T.


-spec pool_name_to_atom(pool_name()) -> atom().
pool_name_to_atom(PoolName) when is_binary(PoolName) ->
    pool_name_to_atom(erlang:binary_to_atom(PoolName, utf8));
pool_name_to_atom(PoolName) when is_list(PoolName) ->
    pool_name_to_atom(list_to_atom(PoolName));
pool_name_to_atom(PoolName) -> PoolName.
