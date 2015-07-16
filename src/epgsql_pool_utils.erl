-module(epgsql_pool_utils).

-export([
    new_connection/1,
    open_connection/1,
    close_connection/1,
    reconnect/1
]).

-include_lib("epgsql/include/epgsql.hrl").
-include("epgsql_pool.hrl").

new_connection(SectionName) ->
    HostSection = wgconfig:get_string(SectionName, "db_host"),
    % Connection parameters
    Params = #epgsql_params{
        host                 = wgconfig:get_string(HostSection, "host"),
        port                 = wgconfig:get_int(HostSection, "port"),
        username             = wgconfig:get_string(HostSection, "username"),
        password             = wgconfig:get_string(HostSection, "password"),
        database             = wgconfig:get_string(HostSection, "database")
    },
    #epgsql_connection{
        connection_timeout   = wgconfig:get_int(SectionName, "connection_timeout"),
        query_timeout        = wgconfig:get_int(SectionName, "query_timeout"),
        params=Params
    }.

open_connection(State) ->
    Params = State#epgsql_connection.params,
    lager:info("Connect ~p", [Params]),
    #epgsql_params{
        host               = Host,
        port               = Port,
        username           = Username,
        password           = Password,
        database           = Database
    } = Params,
    ConnectionTimeout = State#epgsql_connection.connection_timeout,

    Res = epgsql:connect(Host, Username, Password, [        
        {port, Port},
        {database, Database},
        {timeout, ConnectionTimeout}
    ]),
    case Res of
        {ok, Sock} ->
            {ok, State#epgsql_connection{
                connection=Sock,
                reconnect_attempt=0}};
        {error, Reason} ->
            lager:error("Connect fail: ~p", [Reason]),
            {error, State}
    end.

close_connection(State) ->
    Connection = State#epgsql_connection.connection,
    epgsql:close(Connection),
    #epgsql_connection{connection = undefined}.

reconnect(#epgsql_connection{
        reconnect_attempt = R,
        reconnect_timeout = T} = State) ->
    case T > ?MAX_RECONNECT_TIMEOUT of
        true ->
            reconnect_after(R, ?MIN_RECONNECT_TIMEOUT, T),
            State#epgsql_connection{reconnect_attempt = R + 1};
        _ ->
            T2 = exponential_backoff(R, ?MIN_RECONNECT_TIMEOUT),
            reconnect_after(R, ?MIN_RECONNECT_TIMEOUT, T2),
            State#epgsql_connection{reconnect_attempt=R + 1, reconnect_timeout=T2}
    end.

reconnect_after(R, Tmin, Tmax) ->
    Delay = rand_range(Tmin, Tmax),
    lager:info("Reconnect after ~w ms (attempt ~w)", [Delay, R]),
    erlang:send_after(Delay, self(), open_connection).

rand_range(Min, Max) ->
    max(random:uniform(Max), Min).

exponential_backoff(N, T) ->
    erlang:round(math:pow(2, N)) * T.
