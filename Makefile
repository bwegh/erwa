PROJECT = erwa
CT_SUITES = eunit roundtrip
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

DEPS = cowboy ranch erwalib
dep_erwalib = git https://github.com/bwegh/erwa_lib master


TEST_DEPS = awre
dep_awre = git https://github.com/bwegh/awre master


include erlang.mk
