PROJECT = erwa
CT_SUITES = eunit client roundtrip
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

DEPS = jsx msgpack cowboy ranch
dep_jsx = pkg://jsx
dep_msgpack = https://github.com/msgpack/msgpack-erlang 0.3.2
dep_cowboy = pkg://cowboy 0.10.0
dep_ranch = pkg://ranch 0.10.0


include erlang.mk
