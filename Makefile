build-all:
	rebar get-deps
	rebar compile


compile:
	rebar compile skip_deps=true


run:
	erl -pa ebin -pa deps/*/ebin -boot start_sasl -s epgsql_pool_app test_run


tests:
	rebar compile skip_deps=true
	rebar eunit skip_deps=true
	rebar ct skip_deps=true


clean:
	rebar clean


ct-clean:
	rm -rf logs/*


d:
	dialyzer --src -I include src
