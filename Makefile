build:
	spago -x purerl-tests.dhall build

erl: build
	erlc -o ebin/ output/*/*.erl

test: erl
	erl -pa ebin -noshell -eval "(test_main@ps:main())()" -eval "init:stop()"
