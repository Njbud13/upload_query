APPNAME=upload_query
REBAR=`which rebar3 || echo ./rebar3`

all: deps compile test

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	erl -config config/sys -pa ./_build/default/lib/*/ebin -pa ./_build/default/plugins/*/ebin  -sname $(APPNAME)@localhost -s $(APPNAME) -s sync

remshell:
	erl -remsh $(APPNAME)@localhost

test:
	@( $(REBAR) ct --logdir "ct_run" --sys_config "config/*")

.PHONY: all, deps, compile, test
