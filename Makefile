.PHONY: test

ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar

all: clean get-deps compile xref generate

generate:
	cd rel && .$(REBAR) generate

update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean:
	@ $(REBAR) clean

test:
	@ export ERL_MAX_ETS_TABLES=$MAX_ETS_COUNT
	@$(REBAR) skip_deps=true eunit

edoc:
	@$(REBAR) skip_deps=true doc
