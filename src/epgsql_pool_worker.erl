-module(epgsql_pool_worker).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("epgsql_pool.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-record(state, {pool_name :: atom(),
                connection :: #epgsql_connection{}
               }).

%% Module API

start_link(PoolName0) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    gen_server:start_link(?MODULE, PoolName, []).


%%% gen_server API

init(PoolName) ->
    error_logger:info_message("Init epgsql pool worker: ~p", [PoolName]),
    process_flag(trap_exit, true),
    self() ! open_connection,
    {ok, #state{pool_name = PoolName}}.


handle_call({equery, _, _}, _From, #state{connection = undefined} = State) ->
    {reply, {error, no_connection}, State};

handle_call({equery, Stmt, Params}, _From, State) ->
    ConnState = State#state.connection,
    Conn = ConnState#epgsql_connection.connection,
    %% TStart = os:timestamp(),
    %% TODO: query_timeout
    Result = epgsql:equery(Conn, Stmt, Params),
    %% Time = timer:now_diff(os:timestamp(), TStart),
    {reply, Result, State};

handle_call(Message, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Message, ?MODULE]),
    {reply, ok, State}.


handle_cast(Message, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Message, ?MODULE]),
    {noreply, State}.


handle_info(open_connection, #state{pool_name = PoolName} = State) ->
    ConnectionParams = epgsql_pool_settings:get_connection_params(PoolName),
    case open_connection(ConnectionParams) of
        {ok, Connection} ->
            {noreply, State#state{connection = Connection}};
        {error, Reason, Connection} ->
            error_logger:error_msg("Pool ~p could not to connect to DB:~p", [PoolName, Reason]),
            Connection2 = epgsql_pool_utils:reconnect(Connection),
            {noreply, State#state{connection = Connection2}}
    end;

handle_info({'EXIT', Pid, Reason},
            #state{connection = #epgsql_connection{connection_sock = Sock}} = State)
  when Pid == Sock ->
    error_logger:error_msg("DB Connection ~p EXIT with reason: ~p", [Pid, Reason]),
    Connection = epgsql_pool_utils:close_connection(State#state.connection),
    Connection2 = epgsql_pool_utils:reconnect(Connection),
    {noreply, State#state{connection = Connection2}};

handle_info(Message, State) ->
    error_logger:error_msg("unknown info ~p in ~p ~n", [Message, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    normal.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
