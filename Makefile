REBAR=`which rebar || ./rebar`
.PHONY: all edoc test clean

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

