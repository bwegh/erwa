PROJECT = erwa
CT_SUITES = eunit connect roundtrip callee metaevent
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info


DEPS = cowboy ranch wamper elogger
dep_wamper = git https://github.com/bwegh/wamper master
dep_elogger = git https://github.com/bwegh/elogger master

TEST_DEPS = awre
dep_awre = git https://github.com/bwegh/awre master


include erlang.mk
