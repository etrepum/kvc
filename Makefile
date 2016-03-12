REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
.PHONY: all edoc test clean proper

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@$(REBAR) xref
	@$(REBAR) skip_deps=true eunit

proper:
	@$(REBAR) -C rebar.proper.config get-deps compile
	@$(REBAR) -C rebar.proper.config xref
	@$(REBAR) -C rebar.proper.config skip_deps=true eunit

clean:
	@$(REBAR) clean
