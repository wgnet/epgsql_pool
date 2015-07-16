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

-import(epgsql_pool_utils, [
    new_connection/1,
    open_connection/1,
    close_connection/1,
    reconnect/1
]).

-include("epgsql_pool.hrl").

-include_lib("epgsql/include/epgsql.hrl").

-record(state, {
    config_section :: string(),
    connection     :: #epgsql_connection{},
    connection_timeout  :: non_neg_integer(),
    query_timeout       :: non_neg_integer()
}).

start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

init(SectionName) -> 
    lager:debug("Init epgsql pool worker: ~p", [SectionName]),
    process_flag(trap_exit, true),
    random:seed(os:timestamp()),

    State = #state{
        config_section       = SectionName,
        connection           = new_connection(SectionName)
    },
    erlang:send(self(), open_connection),
    {ok, State}.

handle_call({_Message}, _From, #state{connection = undefined} = State) ->
    {reply, {error, no_connection}, State};
handle_call({equery, Stmt, Params}, _From, State) ->
    ConnState = State#state.connection,
    Conn = ConnState#epgsql_connection.connection,
    TStart = os:timestamp(),
    %TODO: query_timeout
    Result = epgsql:equery(Conn, Stmt, Params),
    Time = timer:now_diff(os:timestamp(), TStart),
    lager:debug(
        "Stmt=~p, Params=~p, Time=~p s, Result=~p",
        [Stmt, Params, Time / 1.0e6, Result]),
    {reply, Result, State};
handle_call(Message, From, State) ->
    lager:info(
        "Call / Message: ~p, From: ~p, State: ~p", [Message, From, State]),
    {reply, ok, State}.

handle_cast(Message, State) ->
    lager:info("Cast / Message: ~p, State: ~p", [Message, State]),
    {noreply, State}.

handle_info(open_connection, State) ->
    ConnState = State#state.connection,
    case open_connection(ConnState) of
        {ok, UpdConnState} ->
            lager:debug("Connected: ~p", [UpdConnState]),
            {noreply, State#state{connection = UpdConnState}};
        {error, UpdConnState} ->
            lager:error(
                "Pool ~p could not to connect",
                [State#state.config_section]),
            folsom_metrics:notify({<<"db_connection_errors">>, 1}),
            {noreply, State#state{connection = reconnect(UpdConnState)}}
    end;

handle_info(
        {'EXIT', Pid, Reason},
        #state{connection = #epgsql_connection{connection = C}} = State)
        when Pid == C ->

    lager:debug("EXIT: Connection ~p, Reason: ~p", [Pid, Reason]),
    UpdConnState = close_connection(State#state.connection),
    folsom_metrics:notify({<<"db_connection_errors">>, 1}),
    {noreply, State#state{connection = reconnect(UpdConnState)}};

handle_info(Message, State) ->
    lager:debug("Info / Msg: ~p, State: ~p", [Message, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:debug("Terminate / Reason: ~p, State: ~p", [Reason, State]),
    normal.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- internal functions --
