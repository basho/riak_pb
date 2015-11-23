.PHONY: deps

all: deps compile_all

deps: erl_deps

compile_all: erl_compile java_compile c_compile

clean: erl_clean java_clean c_clean

distclean: clean
	rm -rf dist

release: java_release c_release

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
