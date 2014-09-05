PROJECT = erwa
CT_SUITES = eunit client roundtrip
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

DEPS = jsx msgpack cowboy ranch


include erlang.mk
