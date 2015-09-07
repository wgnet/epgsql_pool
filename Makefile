compile:
	rebar compile


run:
	erl -pa ebin -pa deps/*/ebin -boot start_sasl -s epgsql_pool_app test_run


ct:
	rebar ct skip_deps=true


ct-clean:
	rm -rf logs/*
