.PHONY: all app clean distclean doc shell test

ERL ?= erl
REBAR ?= ./rebar

all: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	rm -rf .eunit

rebar:
	wget "https://github.com/rebar/rebar/releases/download/2.5.1/rebar" -O $@
	chmod +x $@

shell:
	$(ERL) -smp -pa ebin/ -pa deps/*/ebin/

test: $(REBAR)
	$(REBAR) eunit
