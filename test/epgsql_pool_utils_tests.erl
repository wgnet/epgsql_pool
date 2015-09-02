-module(epgsql_pool_utils_tests).

-include("epgsql_pool.hrl").
-include_lib("eunit/include/eunit.hrl").


pool_name_to_atom_test() ->
    ?assertEqual(my_pool, epgsql_pool_utils:pool_name_to_atom(my_pool)),
    ?assertEqual(my_pool, epgsql_pool_utils:pool_name_to_atom("my_pool")),
    ?assertEqual(my_pool, epgsql_pool_utils:pool_name_to_atom(<<"my_pool">>)),
    ok.
