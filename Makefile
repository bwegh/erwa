PROJECT = erwa
CT_SUITES = eunit connect roundtrip callee metaevent
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

COMPILE_FIRST = erwa_middleware.erl

DEPS = cowboy ranch wamper
dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_ranch = git https://github.com/ninenines/ranch.git master
dep_wamper = git https://github.com/comtihon/wamper.git master


TEST_DEPS = awre meck
dep_awre = git https://github.com/comtihon/awre.git master
dep_meck = git https://github.com/eproxus/meck.git master


include erlang.mk