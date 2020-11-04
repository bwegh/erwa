PROJECT = erwa
CT_SUITES = eunit connect roundtrip callee metaevent
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info 


DEPS = cowboy wamper lager
%%dep_cowboy = git https://github.com/ninenines/cowboy master
dep_cowboy_commit = master
dep_wamper = git https://github.com/bwegh/wamper master
dep_lager = git https://github.com/erlang-lager/lager 3.8.1



TEST_DEPS = awre
dep_awre = git https://github.com/bwegh/awre master


include erlang.mk
