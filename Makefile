all: build

build: dependencies
	rebar compile

clean:
	rebar clean

dependencies:
	rebar get-deps

prod: build
	erl -boot start_sasl -sname first -pa ebin/ -pa src/ -pa include -pa ebin/ -config collector.config -eval "application:start(collector)."

run: build
	erl -sname first -pa ebin/ -pa deps/bert/ebin -pa deps/mochiweb/ebin -config collector.config -eval "application:start(collector)."

test: build
	rebar eunit

# spawn an erlang shell on a running node
remote:
	erl -sname toto -remsh first@Pomme
