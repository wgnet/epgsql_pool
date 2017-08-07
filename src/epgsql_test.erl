-module(epgsql_test).
-behaviour(gen_server).

-export([test_run/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


test_run() ->
    application:ensure_all_started(epgsql_pool),
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).


init(no_args) ->
    application:set_env(epgsql_pool, connect_listener, self()),
    application:set_env(epgsql_pool, disconnect_listener, ?MODULE),
    self() ! start_pool,
    {ok, no_state}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(start_pool, State) ->
    error_logger:info_msg("Start Pool"),
    Params = #{
        host => "localhost",
        port => 5432,
        username => "test",
        password => "test",
        database => "testdb"
    },
    {ok, _} = epgsql_pool:start(my_pool, 2, 2, Params),
    self() ! do_queries,
    {noreply, State};

handle_info(do_queries, State) ->
    error_logger:info_msg("Do Queries"),
    Qs = [
        "CREATE TABLE category (id bigserial, title text, PRIMARY KEY (id))",
        "CREATE TABLE item (id bigserial, category_id bigint, title text, num int, PRIMARY KEY (id), "
        "FOREIGN KEY (category_id) REFERENCES category (id) ON DELETE SET NULL)",
        "insert into category (title) values ('some'), ('other')",
        "select * from category"
    ],
    lists:foreach(
        fun(Q) ->
            Res = epgsql_pool:query(my_pool, Q),
            error_logger:info_msg("Q:~p~nRes:~p~n", [Q, Res])
        end,
        Qs),
    {noreply, State};

handle_info({epgsql_connect, PoolName, Sock}, State) ->
    error_logger:info_msg("On Connect ~p ~p", [PoolName, Sock]),
    Res = epgsql_pool:query(Sock, "select id from category"),
    error_logger:info_msg("Res:~p", [Res]),
    {noreply, State};

handle_info({epgsql_disconnect, PoolName}, State) ->
    error_logger:info_msg("On Disconnect ~p", [PoolName]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

