-module(epgsql_pool_worker).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("epgsql_pool.hrl").
-include("otp_types.hrl").

-record(state, {pool_name :: atom(),
                connection :: #epgsql_connection{}
               }).

%% Module API

-spec start_link(epgsql_pool:pool_name()) -> gs_start_link_reply().
start_link(PoolName0) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    gen_server:start_link(?MODULE, PoolName, []).


%%% gen_server API

-spec init(gs_args()) -> gs_init_reply().
init(PoolName) ->
    error_logger:info_msg("Init epgsql pool worker: ~p", [PoolName]),
    process_flag(trap_exit, true),
    self() ! open_connection,
    {ok, #state{pool_name = PoolName}}.


-spec handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply().
handle_call({equery, _, _}, _From, #state{connection = undefined} = State) ->
    {reply, {error, no_connection}, State};

handle_call({equery, Stmt, Params}, _From, #state{connection = Connection} = State) ->
    %% TStart = os:timestamp(),
    Sock = Connection#epgsql_connection.connection_sock,
    Result = epgsql:equery(Sock, Stmt, Params),
    %% Time = timer:now_diff(os:timestamp(), TStart),
    {reply, Result, State};

handle_call(Message, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Message, ?MODULE]),
    {reply, ok, State}.


-spec handle_cast(gs_request(), gs_state()) -> gs_cast_reply().
handle_cast(Message, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Message, ?MODULE]),
    {noreply, State}.


-spec handle_info(gs_request(), gs_state()) -> gs_info_reply().
handle_info(open_connection, #state{pool_name = PoolName} = State) ->
    ConnectionParams = epgsql_pool_settings:get_connection_params(PoolName),
    case epgsql_pool_utils:open_connection(ConnectionParams) of
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


-spec terminate(terminate_reason(), gs_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), term(), term()) -> gs_code_change_reply().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
