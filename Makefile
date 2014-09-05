PROJECT = erwa
CT_SUITES = eunit client roundtrip
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

DEPS = jsx msgpack cowboy ranch
dep_msgpack = git https://github.com/msgpack/msgpack-erlang 0.3.2

include erlang.mk
