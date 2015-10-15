.PHONY: deps

all: deps compile_all

deps: erl_deps

compile_all: erl_compile python_compile python3_compile java_compile c_compile

clean: erl_clean python_clean python3_clean java_clean c_clean

distclean: clean
	rm -rf dist

release: python_release python3_release java_release c_release

# Erlang-specific build steps
DIALYZER_APPS = kernel stdlib erts crypto compiler hipe syntax_tools
include tools.mk

erl_deps:
	@${REBAR} get-deps

erl_compile:
	@${REBAR} compile

erl_clean:
	@${REBAR} clean

compile: erl_compile # Hack for tools.mk

# Python 2.x specific build steps
python_compile:
	@echo "==> Python (compile)"
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python2.7 ./setup.py build_messages build --build-base=python

python_clean:
	@echo "==> Python (clean)"
	@python2.7 ./setup.py clean --build-base=python clean_messages
	@rm -rf *.pyc riak_pb/*_pb2.py riak_pb/*.pyc riak_pb.egg-info python

python_release: python_clean
ifeq ($(RELEASE_GPG_KEYNAME),)
	@echo "RELEASE_GPG_KEYNAME must be set to release/deploy"
else
	@echo "==> Python (release)"
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python2.7 setup.py build_messages build --build-base=python
	@python2.7 setup.py build --build-base=python bdist_egg upload -s -i $(RELEASE_GPG_KEYNAME)
	@python2.7 setup.py clean --build-base=python clean_messages
	@rm -rf *.pyc riak_pb/*_pb2.py riak_pb/*.pyc riak_pb.egg-info python
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python2.7 setup.py build_messages build --build-base=python
	@python2.7 setup.py build --build-base=python sdist upload -s -i $(RELEASE_GPG_KEYNAME)
	@python2.6 setup.py clean --build-base=python clean_messages
	@rm -rf riak_pb/*_pb2.pyc *.pyc python_riak_pb.egg-info python
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python2.6 setup.py build_messages build --build-base=python
	@python2.6 setup.py build --build-base=python bdist_egg upload -s -i $(RELEASE_GPG_KEYNAME)
endif

python_install: python_compile
	@echo "==> Python (install)"
	@python2.7 ./setup.py build_messages build --build-base=python install

# Python 3.x specific build steps
python3_compile:
	@echo "==> Python 3 (compile)"
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python3 setup.py build_messages build --build-base=python3

python3_clean:
	@echo "==> Python 3 (clean)"
	@python3 setup.py clean --build-base=python3 clean_messages
	@rm -rf riak_pb/*_pb2.py riak_pb/__pycache__ __pycache__ python3_riak_pb.egg-info python3

python3_release: python3_clean
ifeq ($(RELEASE_GPG_KEYNAME),)
	@echo "RELEASE_GPG_KEYNAME must be set to release/deploy"
else
	@echo "==> Python 3 (release)"
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python3.4 setup.py build_messages build --build-base=python3
	@python3.4 setup.py build --build-base=python3 bdist_egg upload -s -i $(RELEASE_GPG_KEYNAME)
	@python3.4 setup.py clean --build-base=python3 clean_messages
	@rm -rf riak_pb/*_pb2.py riak_pb/__pycache__ __pycache__ python3_riak_pb.egg-info python3
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python3.4 setup.py build_messages build --build-base=python3
	@python3.4 setup.py build --build-base=python3 sdist upload -s -i $(RELEASE_GPG_KEYNAME)
	@python3.4 setup.py clean --build-base=python3 clean_messages
	@rm -rf riak_pb/*_pb2.py riak_pb/__pycache__ __pycache__ python3_riak_pb.egg-info python3
	@protoc -Isrc --python_out=riak_pb src/*.proto
	@python3.3 setup.py build_messages build --build-base=python3
	@python3.3 setup.py build --build-base=python3 bdist_egg upload -s -i $(RELEASE_GPG_KEYNAME)
endif

python3_install: python3_compile
	@echo "==> Python 3 (install)"
	@python3 setup.py build_messages build --build-base=python3 install

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
	@mvn deploy
endif

# C specific build steps
PROTOC	 = protoc-c
PROTOS	:= $(wildcard src/*.proto)
C_DIR	 = c
C_FILES	:= $(patsubst src/%.proto,$(C_DIR)/%.pb-c.c,$(PROTOS))
H_FILES	:= $(patsubst src/%.proto,$(C_DIR)/%.pb-c.h,$(PROTOS))
C_PREFIX := /usr/local/riak_pb_c

c_compile: c_announce c_protoc_check $(C_DIR) $(C_FILES) $(H_FILES)

c_announce:
	@echo "==> C (compile)"
	@true

c_protoc_check: PROTOC-exists
PROTOC-exists: ; @which $(PROTOC) > /dev/null

$(C_DIR):
	@mkdir -p $(C_DIR)

$(C_DIR)/%.pb-c.c $(C_DIR)/%.pb-c.h: src/%.proto
	@echo "Generating $@ from $<"
	@$(PROTOC) -Isrc $< --c_out=$(C_DIR)

c_clean:
	@echo "==> C (clean)"
	@rm -rf $(C_DIR)

c_release: c_compile
	@echo "==> C (release)"
	@echo "Installing in $(C_PREFIX)"
	@mkdir -p $(C_PREFIX)
	@mkdir -p $(C_PREFIX)/include
	@cp -p $(C_DIR)/*.c $(C_PREFIX)
	@cp -p $(C_DIR)/*.h $(C_PREFIX)/include
