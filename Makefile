.PHONY: deps

ERLFLAGS= -pa $(PWD)/.eunit -pa $(PWD)/ebin -pa $(PWD)/deps/*/ebin

DEPS_PLT=$(PWD)/.deps_plt
DEPS=erts kernel stdlib
REBAR=./rebar

all: deps compile modules

modules:

compile: modules
	@$(REBAR) compile

app: modules
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) get-deps

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS)

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

clean-modules:

clean: clean-modules
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps
	- rm -rf $(DEPS_PLT)

test: app
	@$(REBAR) eunit skip_deps=true

console:
	exec erl $(ERLFLAGS) \
	  -eval "application:start(etz)"
