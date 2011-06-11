REBAR=`which rebar || ./rebar`

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

