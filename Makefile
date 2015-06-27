PROJECT = erwa
CT_SUITES = connect roundtrip callee metaevent
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

COMPILE_FIRST = erwa_middleware.erl

DEPS = cowboy ranch wamper
dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_wamper = git https://github.com/bwegh/wamper master


TEST_DEPS = awre
dep_awre = git https://github.com/bwegh/awre master


include erlang.mk
