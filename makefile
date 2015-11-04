REBAR = `which rebar`

all: clean deps build

deps:
	@( $(REBAR) get-deps )

build:
	@( $(REBAR) compile )

rebuild: clean build

clean:
	@( rm -f ./ebin/data.dat )
	@( $(REBAR) clean )

run:
	@( erl -pa ./ebin ./deps/*/ebin -s trafficLight start )

test:
	@( $(REBAR) eunit skip_deps=true )

good: all test run

release: all
	@( $(REBAR) generate )

.PHONY: all deps build rebuild clean run test good release
