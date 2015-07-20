%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2015 14:11
%%%-------------------------------------------------------------------
-module(erwa_realms_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
  {ok, _} = erwa_realms:start(),
  {ok, stopped} = erwa_realms:stop().

environment_test() ->
  application:set_env(erwa, router_middleware, [erwa_mw_allow]),
  [erwa_mw_allow] = application:get_env(erwa, router_middleware, [erwa_mw_default]),
  application:unset_env(erwa, router_middleware),
  [erwa_mw_default] = application:get_env(erwa, router_middleware, [erwa_mw_default]).

garbage_test() ->
  {ok, Pid} = erwa_realms:start(),
  ignored = gen_server:call(erwa_realms, some_garbage),
  ok = gen_server:cast(erwa_realms, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = erwa_realms:stop().

add_remove_test() ->  %TODO functional?
  {ok, _} = erwa_routing_sup:start_link(),
  {ok, _} = erwa_realms:start(),
  Name1 = <<"com.doesnotexist.wamp">>,
  Name2 = <<"com.doesnotexist.pamw">>,
  MWL = [erwa_mw_allow],
  ok = erwa_realms:set_autocreate(false),
  0 = get_tablesize(),
  {error, not_found} = erwa_realms:get_routing(Name1),
  0 = get_tablesize(),
  {error, not_running} = erwa_realms:kill(Name1),
  0 = get_tablesize(),
  ok = erwa_realms:add(Name1),
  2 = get_tablesize(),
  {error, already_exists} = erwa_realms:add(Name1),
  2 = get_tablesize(),
  ok = erwa_realms:add(Name2, MWL),
  4 = get_tablesize(),
  {ok, _} = erwa_realms:get_routing(Name1),
  {ok, _} = erwa_realms:get_middleware_list(Name1),
  {ok, _} = erwa_realms:get_routing(Name2),
  {ok, killed} = erwa_realms:kill(Name1),
  ok = ensure_tablesize(2, 5000),
  {error, not_found} = erwa_realms:get_routing(Name1),
  {ok, shutting_down} = erwa_realms:shutdown(Name2),
  ok = ensure_tablesize(0, 5000),
  {error, not_found} = erwa_realms:get_routing(Name2),
  ok = erwa_realms:set_autocreate(true),
  {ok, _} = erwa_realms:get_routing(Name1),
  ok = erwa_realms:set_autocreate(false),
  2 = get_tablesize(),
  {ok, killed} = erwa_realms:kill(Name1),
  ok = ensure_tablesize(0, 5000),
  timeout = ensure_tablesize(5, 10),
  {ok, stopped} = erwa_realms:stop().

%% @private
get_tablesize() ->
  Pid = whereis(erwa_realms),
  Tables = ets:all(),
  [Table] = lists:filter(fun(T) -> ets:info(T, owner) == Pid end, Tables),
  ets:info(Table, size).

%% @private
ensure_tablesize(_Number, MaxTime) when MaxTime =< 0 ->
  timeout;
ensure_tablesize(Number, MaxTime) ->
  case get_tablesize() of
    Number -> ok;
    _ ->
      timer:sleep(10),
      NewTime = MaxTime - 10,
      ensure_tablesize(Number, NewTime)
  end.
