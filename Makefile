.PHONY: deps

REBAR=./rebar

all: deps compile modules

modules:

compile: modules
	@$(REBAR) compile

app: modules
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) get-deps

clean-modules:

clean: clean-modules
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

test: app
	@$(REBAR) eunit skip_deps=true

console:
	exec erl -pa $(PWD)/ebin \
	  -pa $(PWD)/deps/*/ebin \
	  -eval "application:start(etz)"
