compile:
	rebar compile


run:
	erl -pa ebin -pa deps/*/ebin -boot start_sasl -s epgsql_pool_app test_run


tests:
	rebar compile skip_deps=true
	rebar eunit skip_deps=true
	rebar ct skip_deps=true


ct-clean:
	rm -rf logs/*


d:
	dialyzer --src -I include src
