-module(epgsql_pool_settings).
-behavior(gen_server).

-export([start_link/0, get_connection_params/1, set_connection_params/2, get/1, set/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("epgsql_pool.hrl").
-include("otp_types.hrl").


%%% module API

-spec start_link() -> gs_start_link_reply().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec get_connection_params(epgsql_pool:pool_name()) -> #epgsql_connection_params{}.
get_connection_params(PoolName) ->
    Key = {connection, epgsql_pool_utils:pool_name_to_atom(PoolName)},
    case ets:lookup(?MODULE, Key) of
        [] -> throw({connection_params_not_found, PoolName});
        [{{connection, PoolName}, ConnectionParams}] -> ConnectionParams
    end.


-spec set_connection_params(egpsql_pool:pool_name(), #epgsql_connection_params{}) -> ok.
set_connection_params(PoolName, Params) ->
    Key = {connection, epgsql_pool_utils:pool_name_to_atom(PoolName)},
    gen_server:call(?MODULE, {save, Key, Params}).


-spec get(atom()) -> integer().
get(Key) ->
    case ets:lookup(?MODULE, {settings, Key}) of
        [] -> throw({settings_not_found, Key});
        [{_, Value}] -> Value
    end.


-spec set(atom(), integer()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {save, {settings, Key}, Value}).


%%% gen_server API

-spec init(gs_args()) -> gs_init_reply().
init([]) ->
    T = ets:new(?MODULE, [protected, named_table]),
    ets:insert(T, {{settings, connection_timeout}, 10000}),
    ets:insert(T, {{settings, query_timeout}, 10000}),
    ets:insert(T, {{settings, pooler_get_worker_timeout}, 10000}),
    ets:insert(T, {{settings, max_reconnect_timeout}, 5000}),
    ets:insert(T, {{settings, min_reconnect_timeout}, 100}),
    ets:insert(T, {{settings, keep_alive_timeout}, 60000}),
    {ok, T}.


-spec handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply().
handle_call({save, Key, Value}, _From, Table) ->
    ets:insert(Table, {Key, Value}),
    {reply, ok, Table};

handle_call(Any, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_cast(gs_request(), gs_state()) -> gs_cast_reply().
handle_cast(Any, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_info(gs_request(), gs_state()) -> gs_info_reply().
handle_info(stop, State) ->
    {stop, normal, State};

handle_info(Request, State) ->
    error_logger:error_msg("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


-spec terminate(terminate_reason(), gs_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), term(), term()) -> gs_code_change_reply().
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
