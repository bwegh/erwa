PROJECT = erwa
CT_SUITES = eunit connect roundtrip callee metaevent
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info


DEPS = cowboy ranch wamper
dep_cowboy = git https://github.com/ninenines/cowboy.git 06bd01d35ef808ccc5cab2a0285579d60f73df40
#dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_ranch = git https://github.com/ninenines/ranch.git 9508d521c4f8ffd88374281c8e37ebaaa262a784
#dep_ranch = git https://github.com/ninenines/ranch.git master
dep_wamper = git https://github.com/bwegh/wamper master


TEST_DEPS = awre
dep_awre = git https://github.com/bwegh/awre master


include erlang.mk
