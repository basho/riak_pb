.PHONY: deps

all: deps compile

deps: erl_deps

compile: erl_compile python_compile java_compile

clean: erl_clean python_clean java_clean

distclean: clean
	rm -rf dist

release: python_release java_release

test: erl_test

# Erlang-specific build steps
erl_deps:
	@./rebar get-deps

erl_compile:
	@./rebar compile

erl_clean:
	@./rebar clean

erl_test: erl_compile
	@./rebar eunit skip_deps=true

REPO = riak_pb
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

check_plt: erl_deps erl_compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

build_plt: erl_deps erl_compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: erl_deps erl_compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin

cleanplt:
	@echo
	@echo "Are you sure?  It could take a long time to rebuild."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

# Python specific build steps
python_compile:
	@echo "==> Python (compile)"
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@./setup.py build

python_clean:
	@echo "==> Python (clean)"
	@rm -f riak_pb/*_pb2.py
	@./setup.py clean

python_release: python_compile
	@echo "==> Python (release)"
	@python2.6 setup.py bdist_egg upload
	@python2.7 setup.py bdist_egg upload
	@python2.6 setup.py sdist upload

# Java specific build steps
java_compile:
	@echo "==> Java (compile)"
	@mvn install

java_clean:
	@echo "==> Java (clean)"
	@mvn clean

java_release:
	@echo "==> Java"
ifeq ($(RELEASE_GPG_KEYNAME),)
	@echo "RELEASE_GPG_KEYNAME must be set to release/deploy"
else
	@mvn clean
	@mvn deploy -Dgithub.downloads=true
endif
