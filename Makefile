PROJECT = erwa
CT_SUITES = eunit connect roundtrip callee metaevent
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info 


DEPS = cowboy wmper lager
%%dep_cowboy = git https://github.com/ninenines/cowboy master
dep_cowboy_commit = master
dep_wamper = git https://github.com/bwegh/wamper master
dep_lager = git https://github.com/basho/lager 3.2.4



TEST_DEPS = awre
dep_awre = git https://github.com/bwegh/awre master


include erlang.mk
