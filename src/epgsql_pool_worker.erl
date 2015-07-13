-module(epgsql_pool_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("epgsql_pool.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(MAX_RECONNECT_TIMEOUT, 1000*30).
-define(MIN_RECONNECT_TIMEOUT, 200).

-record(state, {
    connection            :: pid(),
    params                :: #epgsql_params{},
    reconnect_attempt = 0 :: non_neg_integer(),
    reconnect_timeout = 0 :: non_neg_integer()
}).

start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

init(Params) -> 
    process_flag(trap_exit, true),
    random:seed(now()),
    self() ! open_connection,
    {ok, #state{params=Params}}.

handle_call({_Message}, _From, #state{connection = undefined} = State) ->
    {reply, {error, no_connection}, State};
handle_call({equery, Stmt, Params}, From, State) ->
    TStart = now(),
    Result = epgsql:equery(State#state.connection, Stmt, Params),
    Time = timer:now_diff(now(), TStart),
    lager:debug(
        "Stmt=~p, Params=~p, Time=~p ms, Result=~p",
        [Stmt, Params, Time / 1.0e3, Result]),
    {reply, Result, State};
handle_call(Message, From, State) ->
    lager:info(
        "Call / Message: ~p, From: ~p, State: ~p", [Message, From, State]),
    {reply, ok, State}.

handle_cast(Message, State) ->
    lager:info("Cast / Message: ~p, State: ~p", [Message, State]),
    {noreply, State}.

handle_info(open_connection, State) ->
    case open_connection(State) of
        {ok, UpdState} ->
            {noreply, UpdState};
        {error, UpdState} ->
            {noreply, reconnect(UpdState)}
    end;

handle_info({'EXIT', Pid, Reason}, #state{connection = C} = State) when Pid == C ->
    lager:error("Exit with reason: ~p", [Reason]),
    close_connection(State),
    {noreply, reconnect(State)};

handle_info(Message, State) ->
    lager:debug("Info / Msg: ~p, State: ~p", [Message, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:debug("Terminate / Reason: ~p, State: ~p", [Reason, State]),
    normal.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- internal functions --

open_connection(#state{params = Params} = State) ->

    #epgsql_params{
        host               = Host,
        port               = Port,
        username           = Username,
        password           = Password,
        database           = Database,
        connection_timeout = ConnectionTimeout
    } = Params,

    Res = epgsql:connect(Host, Username, Password, [        
        {port, Port},
        {database, Database},
        {timeout, ConnectionTimeout}
    ]),
    case Res of
        {ok, Sock} ->
            {ok, State#state{
                connection=Sock,
                reconnect_attempt=0}};
        {error, Reason} ->
            lager:error("Connect fail: ~p", [Reason]),
            {error, State}
    end.

close_connection(State) ->
    Connection = State#state.connection,
    epgsql:close(Connection),
    #state{connection = undefined}.

reconnect(#state{
        reconnect_attempt = R,
        reconnect_timeout = T} = State) ->
    case T > ?MAX_RECONNECT_TIMEOUT of
        true ->
            reconnect_after(R, ?MIN_RECONNECT_TIMEOUT, T),
            State#state{reconnect_attempt = R + 1};
        _ ->
            T2 = exponential_backoff(R, ?MIN_RECONNECT_TIMEOUT),
            reconnect_after(R, ?MIN_RECONNECT_TIMEOUT, T2),
            State#state{reconnect_attempt=R + 1, reconnect_timeout=T2}
    end.

reconnect_after(R, Tmin, Tmax) ->
    Delay = rand_range(Tmin, Tmax),
    lager:error("Reconnect after ~w ms (attempt ~w)", [Delay, R]),
    erlang:send_after(Delay, self(), open_connection).

rand_range(Min, Max) ->
    max(random:uniform(Max), Min).

exponential_backoff(N, T) ->
    erlang:round(math:pow(2, N)) * T.
