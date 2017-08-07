-module(epgsql_pool_worker).

-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("epgsql_pool.hrl").
-include("otp_types.hrl").

-record(state, {pool_name :: atom(),
                connection :: #epgsql_connection{} | undefined,
                keep_alive_query_ref :: reference() | undefined, % ref to async keep-alive query to DB
                send_keep_alive_timer :: reference(), % timer to send keep-alive query to DB
                no_reply_keep_alive_timer :: reference() % timer to wait for reply from DB
               }).


 %% Module API

-spec start_link(epgsql_pool:pool_name()) -> gs_start_link_reply().
start_link(PoolName0) ->
    PoolName = epgsql_pool_utils:pool_name_to_atom(PoolName0),
    gen_server:start_link(?MODULE, PoolName, []).


-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).


%%% gen_server API

-spec init(gs_args()) -> gs_init_reply().
init(PoolName) ->
    process_flag(trap_exit, true),
    herd_rand:init_crypto(),
    self() ! open_connection,
    {ok, #state{pool_name = PoolName,
                send_keep_alive_timer = make_ref(), % no need to check for undefined in cancel_timer
                no_reply_keep_alive_timer = make_ref()}}.


-spec handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply().
handle_call(stop, _From, #state{connection = Connection,
                                send_keep_alive_timer = Send_KA_Timer,
                                no_reply_keep_alive_timer = NR_KA_Timer} = State) ->
    Connection2 = epgsql_pool_utils:close_connection(Connection),
    erlang:cancel_timer(Send_KA_Timer),
    erlang:cancel_timer(NR_KA_Timer),
    {stop, normal, ok, State#state{connection = Connection2}};

handle_call(_, _From, #state{connection = undefined} = State) ->
    {reply, {error, no_connection}, State};

handle_call(_, _From, #state{connection = #epgsql_connection{sock = undefined}} = State) ->
    {reply, {error, no_connection}, State};

handle_call(get_sock, _From,
            #state{connection = #epgsql_connection{sock = Sock}} = State) ->
    {reply, Sock, State};

handle_call({equery, Stmt, Params}, _From,
            #state{connection = #epgsql_connection{sock = Sock}} = State) ->
    Reply = case process_info(Sock, status) of
                undefined -> {error, no_connection};
                {status, _} -> epgsql:equery(Sock, Stmt, Params)
            end,
    {reply, Reply, State};

handle_call({squery, Stmt}, _From,
            #state{connection = #epgsql_connection{sock = Sock}} = State) ->
    Reply = case process_info(Sock, status) of
                undefined -> {error, no_connection};
                {status, _} -> epgsql:squery(Sock, Stmt)
            end,
    {reply, Reply, State};

handle_call(cancel, _From, #state{connection = #epgsql_connection{sock = Sock}} = State) ->
    epgsql:cancel(Sock),
    {reply, ok, State};

handle_call(Message, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Message, ?MODULE]),
    {reply, ok, State}.


-spec handle_cast(gs_request(), gs_state()) -> gs_cast_reply().
handle_cast(Message, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Message, ?MODULE]),
    {noreply, State}.


-spec handle_info(gs_request(), gs_state()) -> gs_info_reply().
handle_info(open_connection, #state{pool_name = PoolName, connection = Connection,
                                    send_keep_alive_timer = Send_KA_Timer} = State) ->
    case epgsql_pool_utils:open_connection(PoolName, Connection) of
        {ok, Connection2} ->
            {ok, KeepAliveTimeout} = application:get_env(epgsql_pool, keep_alive_timeout),
            erlang:cancel_timer(Send_KA_Timer),
            Send_KA_Timer2 = erlang:send_after(KeepAliveTimeout, self(), keep_alive),
            self() ! on_connect,
            {noreply, State#state{connection = Connection2, send_keep_alive_timer = Send_KA_Timer2}};
        {error, Reason, Connection3} ->
            error_logger:error_msg("Pool:~p, Worker:~p could not to connect to DB:~p", [PoolName, self(), Reason]),
            Connection4 = epgsql_pool_utils:reconnect(Connection3),
            {noreply, State#state{connection = Connection4}}
    end;

handle_info(on_connect, #state{pool_name = PoolName} = State) ->
    case application:get_env(epgsql_pool, on_connect_callback) of
        undefined -> ignore;
        {ok, undefined} -> ignore;
        {ok, {M, F}} -> M:F(PoolName)
    end,
    {noreply, State};

handle_info(on_disconnect, #state{pool_name = PoolName} = State) ->
    case application:get_env(epgsql_pool, on_disconnect_callback) of
        undefined -> ignore;
        {ok, undefined} -> ignore;
        {ok, {M, F}} -> M:F(PoolName)
    end,
    {noreply, State};

handle_info(keep_alive, #state{connection = #epgsql_connection{sock = undefined}} = State) ->
    do_nothing,
    {noreply, State};

handle_info(keep_alive, #state{connection = #epgsql_connection{sock = Sock},
                               no_reply_keep_alive_timer = NR_KA_Timer} = State) ->
    %% send async keep-alive query to DB
    KA_Ref = epgsqli:squery(Sock, "SELECT 1"),

    {ok, QueryTimeout} = application:get_env(epgsql_pool, query_timeout),
    erlang:cancel_timer(NR_KA_Timer),
    NR_KA_Timer2 = erlang:send_after(QueryTimeout, self(), no_reply_to_keep_alive),
    {noreply, State#state{keep_alive_query_ref = KA_Ref, no_reply_keep_alive_timer = NR_KA_Timer2}};

handle_info({_Pid, Ref, done}, #state{keep_alive_query_ref = Ref,
                                      send_keep_alive_timer = Send_KA_Timer,
                                      no_reply_keep_alive_timer = NR_KA_Timer} = State) ->
    %% got reply to asycn keep-alive query from DB
    {ok, KeepAliveTimeout} = application:get_env(epgsql_pool, keep_alive_timeout),
    erlang:cancel_timer(Send_KA_Timer),
    erlang:cancel_timer(NR_KA_Timer),
    Send_KA_Timer2 = erlang:send_after(KeepAliveTimeout, self(), keep_alive),
    {noreply, State#state{send_keep_alive_timer = Send_KA_Timer2}};

handle_info({_Pid, Ref, _Reply}, #state{keep_alive_query_ref = Ref} = State) ->
    do_nothing,
    {noreply, State};

handle_info(no_reply_to_keep_alive, #state{connection = Connection,
                                           send_keep_alive_timer = Send_KA_Timer,
                                           no_reply_keep_alive_timer = NR_KA_Timer} = State) ->
    %% no reply to asycn keep-alive query from DB
    error_logger:error_msg("DB Connection, no_reply_to_keep_alive"),
    erlang:cancel_timer(Send_KA_Timer),
    erlang:cancel_timer(NR_KA_Timer),
    Connection2 = epgsql_pool_utils:close_connection(Connection),
    Connection3 = epgsql_pool_utils:reconnect(Connection2),
    self() ! on_disconnect,
    {noreply, State#state{connection = Connection3}};

handle_info({'EXIT', _Sock, normal},
            #state{connection = #epgsql_connection{sock = undefined}} = State) ->
    do_nothing,
    {noreply, State};

handle_info({'EXIT', Sock, Reason},
    #state{connection = #epgsql_connection{sock = Sock} = Connection} = State) ->
    error_logger:error_msg("DB Connection ~p~nEXIT with reason:~p", [Connection, Reason]),
    Connection2 = epgsql_pool_utils:close_connection(Connection),
    Connection3 = epgsql_pool_utils:reconnect(Connection2),
    self() ! on_disconnect,
    {noreply, State#state{connection = Connection3}};

handle_info({'EXIT', _Sock, econnrefused},
    #state{connection = #epgsql_connection{sock = undefined}} = State) ->
    %% reconnect is already running, do nothing
    {noreply, State};

handle_info(Message, State) ->
    error_logger:error_msg("unknown info ~p in ~p ~n", [Message, ?MODULE]),
    {noreply, State}.


-spec terminate(terminate_reason(), gs_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), term(), term()) -> gs_code_change_reply().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
