# Connection pool for PostgreSQL

Connection pool for PostgreSQL based on async version of
[epgsql](https://github.com/epgsql/epgsql) DB driver and
[pooler](https://github.com/seth/pooler) processes pool.

It allows to create several connection pools with its own
DB settings, so different pools can work with different DBs.

Each pool has many processes, and each process sets its own
DB connection.

To make a query to DB one takes free process from pool.
Process serializes query, sends it to DB, gets reply,
de-serializes it, gives reply to caller, and returns to pool.


## Starting and stoping a pool

To start a pool you should call **epgsql_pool:start/4**
with arguments:

 - pool name _atom() | string() | binary()_
 - number of connections _integer()_
 - maximum number of connections _integer()_
 - connection settings _map()_

```
Params = #{host => "localhost",
           port => 5432,
           username => "someuser",
           password => "pass",
           database => "main_db"},
{ok, _} = epgsql_pool:start(main_pool, 10, 20, Params),
Params2 = #{host => "localhost",
            port => 5432,
            username => "someuser",
            password => "pass",
            database => "other_db"},
{ok, _} = epgsql_pool:start(other_pool, 10, 20, Params2),
```

Connection settings should be either **map()** or **#epgsql_connection_params{}**
record (defined in include/epgsql_pool.hrl).

To stop the pool you should call **epgsql_pool:stop(PoolName)**.


## Connection settings validation

Each process in pool sets its own connection to DB. If it can't set
connection for some reason, it generates error report and tries
to connect after short delay. If connection settings are invalid,
no process in pool is able to set connection, but all of them
generate error reports and try to connect again and again.
This can produce a huge error logs.

To avoid this situation you can validate connection settings before
starting a pool. Just call **epgsql_pool:validate_connection_params/1**.

```
1> Params = #{host => "localhost",
1>                port => 5432,
1>                username => "test",
1>                password => "test",
1>                database => "testdb"}.
2> epgsql_pool:validate_connection_params(Params).
ok
3> epgsql_pool:validate_connection_params(Params#{password := "123"}).
{error,invalid_password}
4> epgsql_pool:validate_connection_params(Params#{database := "some"}).
{error,{error,fatal,<<"3D000">>,
              <<"database \"some\" does not exist">>,[]}}
```

Connection settings should be either **map()** or **#epgsql_connection_params{}**.


## Send query to DB

To send a query to DB you should call one of functions **epgsql_pool:query/2, /3, /4**
with arguments:

 - pool name _atom() | string() | binary()_
 - SQL-query _io_list()_
 - query params (optional) _[term()]_
 - additional settings (optional) _[proplists:option()]_

SQL-query format, query params and reply are the same as
[epgsql](https://github.com/epgsql/epgsql) provides.
See driver documentation.

No need to work with connection pool directly.

```
5> epgsql_pool:query(my_pool, "INSERT INTO category (id, title) VALUES (1, 'My Category'), (2, 'Other Category')").
{ok,2}
6> epgsql_pool:query(my_pool, "INSERT INTO category (id, title) VALUES (3, 'Next Category') RETURNING id").
{ok,1,[{column,<<"id">>,int8,8,-1,1}],[{3}]}
7> epgsql_pool:query(my_pool, "SELECT * FROM category").
{ok,[{column,<<"id">>,int8,8,-1,1},
     {column,<<"title">>,text,-1,-1,1}],
    [{1,<<"My Category">>},
     {2,<<"Other Category">>},
     {3,<<"Next Category">>}]}
```

There is a time limit for query, 10 seconds by default. If library doesn't get a reply
from DB during this time, it cancels query and returns {error, timeout}.

```
8> epgsql_pool:query(my_pool, "select pg_sleep(100)").
{error,timeout}
```

This timeout allows prevent pool overload. Performing query blocks one process from a pool.
Long query blocks process for a long time. Many long queries can block all processes in pool.

You can change query timeout for the query.

```
9> epgsql_pool:query(my_pool, "select pg_sleep(10)", [], [{timeout, 15000}]).
{ok,[{column,<<"b">>,void,4,-1,0}],[{<<>>}]}
```

Timeout are in milliseconds.  You can set it as {timeout, infinity},
if you don't want to limit query time.

```
10> epgsql_pool:query(my_pool, "select pg_sleep(10)", [], [{timeout, infinity}]).
{ok,[{column,<<"b">>,void,4,-1,0}],[{<<>>}]}
```

You can change default query timeout, which affects all queries.
See **Settings** section below.


## Transactions

If you want to perform several queries in transaction use function **epgsql_pool:transaction/2**
with arguments:

 - pool name _atom() | string() | binary()_
 - your function _fun()_

Your function gets _Worker_ which is a process from pool. Use it instead of pool name
in epgsql_pool:query calls.

```
epgsql_pool:transaction(my_pool,
    fun(Worker) ->
        Res1 = epgsql_pool:query(Worker, Query1),
        ...
        ResN = epgsql_pool:query(Worker, QueryN)
    end).
```

Any exception thrown inside your function cancels transaction.

```
epgsql_pool:transaction(my_pool,
    fun(Worker) ->
        Res1 = epgsql_pool:query(Worker, Query1),
        ...
        case SomeData of
            GoodResult -> do_something;
            BadResult -> throw(cancel_transaction)
        end,
        ResN = epgsql_pool:query(Worker, QueryN)
    end).
```


## Keep Alive

Sometimes connection to DB breaks, but client side (epgsql driver)
doesn't know about it. If this happens all queries going through this
connection finish with {error, timeout}.

To prevent this situation each process in pool monitors its
connection, regularly performing "keep alive" queries (actually
"SELECT 1").  If process doesn't get reply to "keep alive" it consider
connection broken and reconnects.

This happens inside library and you don't need to worry about it.
But you may want to change "keep alive" interval, which is 60 seconds by
default. See **Settings** section below.


## Reconnect

Sometimes  **epgsql_pool:query** returns {error, reconnecting}.
This means process lost its connection and currently reconnecting.

Its tries to set connection with delay between attempts.
Delay starts with 100 milliseconds and exponentially increments
up to 5 seconds. Number of attempts is not limited.

You may want to change minimum and maximum delay limits.
See **Settings** section below.


## Settings

This library allows to change several parameters relating to pool and intervals.

You can get all possible parameters and their values with call **epgsql_pool:get_settings/0**:

```
3> epgsql_pool:get_settings().
 #{connection_timeout => 10000,
   keep_alive_timeout => 60000,
   max_reconnect_timeout => 5000,
   min_reconnect_timeout => 100,
   pooler_get_worker_timeout => 10000,
   pooler_max_queue => 100,
   query_timeout => 10000}
```

And you can set new values with call **epgsql_pool:set_settings/1**:

```
3> epgsql_pool:set_settings(
 #{keep_alive_timeout => 24 * 3600 * 1000,
   query_timeout => 60000}).
```

You should only set in map parameters you want to change, not all of them.

There are following parameters:

**connection_timeout** -- timeout for settings connection. 10,000 milliseconds by default.

**keep_alive_timeout** -- interval between "keep alive" queries. 60,000 milliseconds by default.

**min_reconnect_timeout** and **max_reconnect_timeout** -- min and max delay between reconnect attempts.
100 and 5,000 milliseconds by default.

**pooler_get_worker_timeout** -- timeout for getting worker from pool.
If there is no free worker in pool
then epgsql\_pool:query returns {error, pool\_overload}.
10,000 milliseconds by default.

**pooler_max_queue** -- pool queue length.
It there are more clients waiting for worker from pool
then epgsql\_pool:query returns {error, pool\_overload}.
100 clients by default.

**query_timeout** -- waiting reply timeout for query to DB.
10,000 milliseconds by default.

Default values could be changed in future versions.
