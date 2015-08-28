-module(epgsql_pool_settings).
-behavior(gen_server).

-export([start_link/0, get_connection_params/1, set_connection_params/2, get/1, set/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("epgsql_pool.hrl").
-include("otp_types.hrl").

-import(epgsql_pool_utils, [pool_name_to_atom/1]).

-record(state, {
          connection_params :: orddict:orddictr(),
          settings :: orddict:orddict()
         }).


%%% module API

-spec start_link() -> gs_start_link_reply().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec get_connection_params(pool_name()) -> #epgsql_connection_params{}. % or throw({connection_params_not_found, PoolName})
get_connection_params(PoolName) ->
    gen_server:call(?MODULE, {get_connection_params, pool_name_to_atom(PoolName)}).


-spec set_connection_params(pool_name(), #epgsql_connection_params{}) -> ok.
set_connection_params(PoolName, Params) ->
    gen_server:call(?MODULE, {set_connection_params, pool_name_to_atom(PoolName), Params}).


-spec get(atom()) -> integer(). % or throw({settings_not_found, Key})
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


-spec set(atom(), integer()) -> ok. % or throw({settings_not_found, Key})
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).


%%% gen_server API

-spec init(gs_args()) -> gs_init_reply().
init([]) ->
    {ok, #state{connection_params = orddict:new(),
                settings = orddict:from_list(
                             [{query_timeout, 10000},
                              {pooler_get_worker_timeout, 1000},
                              {max_reconnect_timeout, 3000},
                              {min_reconnect_timeout, 100}
                             ])}}.


-spec handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply().
handle_call({get_connection_params, PoolName}, _From,
            #state{connection_params = ConnectionParams} = State) ->
    Reply = case orddict:find(PoolName, ConnectionParams) of
                {ok, Params} -> Params;
                error -> throw({connection_params_not_found, PoolName})
            end,
    {reply, Reply, State};

handle_call({set_connection_params, PoolName, Params}, _From,
            #state{connection_params = ConnectionParams} = State) ->
    ConnectionParams2 = orddict:store(PoolName, Params, ConnectionParams),
    {reply, ok, State#state{connection_params = ConnectionParams2}};

handle_call({get, Key}, _From,
            #state{settings = Settings} = State) ->
    Reply = case orddict:find(Key, Settings) of
                {ok, Value} -> Value;
                error -> throw({settings_not_found, Key})
            end,
    {reply, Reply, State};

handle_call({set, Key, Value}, _From,
            #state{settings = Settings} = State) ->
    Settings2 = case orddict:find(Key, Settings) of
                    {ok, _} -> orddict:store(Key, Value, Settings);
                    error -> throw({settings_not_found, Key})
                end,
    {reply, ok, State#state{settings = Settings2}};

handle_call(Any, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_cast(gs_request(), gs_state()) -> gs_cast_reply().
handle_cast(Any, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_info(gs_request(), gs_state()) -> gs_info_reply().
handle_info(Request, State) ->
    error_logger:error_msg("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


-spec terminate(terminate_reason(), gs_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), term(), term()) -> gs_code_change_reply().
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
