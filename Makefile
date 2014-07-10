all:
	@test -d ebin || mkdir ebin
	@erl -make

check: all
	@dialyzer ebin

clean:
	@rm -rf ebin/*.beam erl_crash.dump

test_cases: all
	erl -pa ebin -boot start_sasl -s inets -s er4cli_tests self_test