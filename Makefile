ERL = $(shell which erl)
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin
REBAR=$(shell which rebar)

all: compile test

compile:
	@./rebar compile

test:
	@./rebar eunit skip_deps=true

doc:
	@./rebar doc skip_deps=true

.PHONY: dialyzer typer clean distclean

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

$(DEPSOLVER_PLT):
	- dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
	    --apps erts kernel stdlib crypto

dialyzer: $(DEPSOLVER_PLT)
	dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src

typer: $(DEPSOLVER_PLT)
	typer --plt $(DEPSOLVER_PLT) -r ./src

clean:
	@./rebar clean

distclean: clean
	rm $(DEPSOLVER_PLT)
