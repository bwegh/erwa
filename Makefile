PROJECT = erwa
CT_SUITES = eunit connect roundtrip callee metaevent
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info 

LOCAL_DEPS = mnesia crypto

DEPS = cowboy ranch wamper lager awre
dep_cowboy = git https://github.com/ninenines/cowboy.git
dep_wamper = git https://github.com/bwegh/wamper master
dep_lager = git https://github.com/basho/lager 3.0.2
dep_awre = git https://github.com/bwegh/awre master


TEST_DEPS = awre
dep_awre = git https://github.com/bwegh/awre master

SHELL_OPTS = -sname test

include erlang.mk
