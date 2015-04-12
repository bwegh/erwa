PROJECT = erwa
CT_SUITES = eunit client roundtrip
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

DEPS = cowboy ranch erwalib
dep_erwalib = git https://github.com/bwegh/erwa_lib master

include erlang.mk
