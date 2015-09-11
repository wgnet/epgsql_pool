# Пул соединений для работы с PostgreSQL

Библиотека реализует комплексное решение для взаимодействия эрланг
проекта с базой PostgreSQL на базе асинхронной версию драйвера
[epgsql](https://github.com/epgsql/epgsql) и пула процессов
[pooler](https://github.com/seth/pooler).

Она позволяет создать несколько пулов процессов, каждый со своими
настройками соединения с базой. Так что разные пулы могут работать с
разными базами.

Внутри пула есть несколько процессов, и каждый из них устанавливает
свое соединение с базой и посылает запросы к ней независимо от
остальных процессов.

Чтобы выполнить запрос к базе, берется свободный процесс из пула,
который сериализует запрос, посылает его базе, получает и
десериализует ответ, и возвращается в пул.


## Запуск и остановка пула

Для запуска пула нужно вызывать функцию **epgsql_pool:start/4**
с аргументами:
- имя пула _atom() | string() | binary()_
- число соединений _integer()_
- максимальное число соединений _integer()_
- настройки соединения _map()_

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

Настройки соединения должны быть **map()** или **#epgsql_connection_params{}**
(определена в include/epgsql_pool.hrl).

Для остановки пула нужно вызывать **epgsql_pool:stop(PoolName)**.


## Проверка настроек

Каждый процесс в пуле пытается установить соединение с базой. Если по
каким-то причинам это не удается, то процесс логирует ошибку, и через
короткий промежуток времени снова пытается соединиться.  Если
настройки соединения указаны неправильно, то такие попытки повторяются
бесконечно.  И если процессов в пуле много, то генерируется много
сообщений об ошибках, которые создают большую нагрузку на IO и
записывают много логов.

Поэтому, перед запуском пула рекомендуется проверить правильность настроек.
Это можно сделать вызовом **epgsql_pool:validate_connection_params/1**

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

Здесь тоже настройки должны быть **map()** или **#epgsql_connection_params{}**.

Если настройки оказались неправильными, то, вероятно, вы захотите
сообщить об ошибке и остановить ноду.


## Запрос к базе данных

Для отправки запроса нужно вызывать одну из функций **epgsql_pool:query/2, /3, /4**
с аргументами:
- имя пула _atom() | string() | binary()_
- SQL-запрос _io_list()_
- опционально, параметры запроса _[term()]_
- опционально, дополнительные настройки _[proplists:option()]_

Формат SQL-запроса, параметров к нему, возможные форматы ответа такие же,
как требует драйвер [epgsql](https://github.com/epgsql/epgsql).
Подробности смотрите в документаци драйвера.

Напрямую работать с пулом соединений не нужно, об этом заботится библиотека.

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

Есть ограничение на время выполнения запроса, по умолчанию оно 10 секунд. Если за это время
библиотека не получает ответ от базы, то запрос отменяется, и возвращается {error, timeout}.

```
8> epgsql_pool:query(my_pool, "select pg_sleep(100)").
{error,timeout}
```

Процесс из пула блокируется, пока не получит ответ от базы. Если запрос выполняется долго,
то процесс долго не возвращается в пул. Если послать много таких долгих запросов, то можно
исчерпать весь пул. Для защиты от такой ситуации введен timeout.

Вы можете изменить timeout для конкретного запроса.

```
9> epgsql_pool:query(my_pool, "select pg_sleep(10)", [], [{timeout, 15000}]).
{ok,[{column,<<"b">>,void,4,-1,0}],[{<<>>}]}
```

Или даже указать **{timeout, infinity}**, если вообще не хотите ограничивать время запроса.

```
10> epgsql_pool:query(my_pool, "select pg_sleep(10)", [], [{timeout, infinity}]).
{ok,[{column,<<"b">>,void,4,-1,0}],[{<<>>}]}
```

timeout задается в миллисекундах. И это пока единственная настройка
запроса, которая поддерживается библиотекой. Возможно, в следующих
версиях появятся и другие настройки. (Поэтому 4-й аргумент --
proplist, а не атомарное значение).

Можно изменить и timeout по умолчанию, что повлияет на все запросы.
Смотрите ниже раздел **Настройки**.


## Транзациии

TODO

## keep-alive

TODO

## reconnect

TODO
with exponential backoff

## настройки

TODO

get_settings
set_settings
