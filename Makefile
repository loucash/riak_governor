PROJECT := riak_governor
REBAR := ./rebar
SNAME := $(PROJECT)

ERL := erl
EPATH = -pa ebin -pz deps/*/ebin
TEST_EPATH = -pz deps/*/ebin -I deps/proper/include -pa ebin -pa test
PLT_APPS = $(shell ls $(ERL_LIB_DIR) | grep -v interface | sed -e 's/-[0-9.]*//')
DIALYZER_OPTS= -Wno_undefined_callbacks --fullpath
CQLSH = cqlsh

.PHONY: all build_plt compile configure console deps doc clean depclean distclean dialyze test test-console

all: deps compile data/ring

build_plt:
	@dialyzer --build_plt --apps $(PLT_APPS)

compile:
	$(REBAR) compile

compile-fast:
	$(REBAR) skip_deps=true compile

configure:
	$(REBAR) get-deps compile

console: data/ring
	$(ERL) -sname $(PROJECT) $(EPATH) -config riak_core

deps:
	$(REBAR) get-deps

doc:
	$(REBAR) skip_deps=true doc

clean:
	$(REBAR) skip_deps=true clean

depclean:
	$(REBAR) clean

distclean:
	$(REBAR) clean delete-deps
	@rm -rf logs
	@rm -rf ct_log
	@rm -rf log

dialyze:
	@dialyzer $(DIALYZER_OPTS) -r ebin

start:
	$(ERL) -sname $(PROJECT) $(EPATH) -s $(PROJECT)

test: compile-fast
	$(REBAR) -C rebar.test.config get-deps compile
	$(REBAR) -C rebar.test.config ct skip_deps=true

test-console: test-compile
	@erlc $(TEST_EPATH) -o test test/*.erl
	$(ERL) -sname $(PROJECT)_test  $(TEST_EPATH) -config sys

dev: compile-fast dev-console

dev-clean: cassandra-freya dev

dev-console:
	$(ERL) -sname $(SNAME) $(EPATH) -s freya -config sys -config riak_core

spam:
	@erl -pa deps/*/ebin -pa ebin -config sys -s lager

data/ring:
	@rm -rf data/ring
	@mkdir -p data/ring

rel: all
	$(REBAR) generate

relclean:
	rm -rf rel/riak_governor

devrel: dev1 dev2 dev3

dev1 dev2 dev3:
	mkdir -p dev
	(cd rel && rebar generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

devclean:
	rm -rf dev
