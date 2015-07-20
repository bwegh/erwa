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
-import(erwa_realms,
[
start/0,
stop/0,
set_autocreate/1,
get_routing/1,
kill/1,
add/1,
add/2,
get_middleware_list/1,
shutdown/1
]).

start_stop_test() ->
  {ok, _} = start(),
  {ok, stopped} = stop().

environment_test() ->
  application:set_env(erwa, router_middleware, [erwa_mw_allow]),
  [erwa_mw_allow] = application:get_env(erwa, router_middleware, [erwa_mw_default]),
  application:unset_env(erwa, router_middleware),
  [erwa_mw_default] = application:get_env(erwa, router_middleware, [erwa_mw_default]).

garbage_test() ->
  {ok, Pid} = start(),
  ignored = gen_server:call(?MODULE, some_garbage),
  ok = gen_server:cast(?MODULE, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = stop().

add_remove_test() ->
  {ok, _} = erwa_routing_sup:start_link(),
  {ok, _} = start(),
  Name1 = <<"com.doesnotexist.wamp">>,
  Name2 = <<"com.doesnotexist.pamw">>,
  MWL = [erwa_mw_allow],
  ok = set_autocreate(false),
  0 = get_tablesize(),
  {error, not_found} = get_routing(Name1),
  0 = get_tablesize(),
  {error, not_running} = kill(Name1),
  0 = get_tablesize(),
  ok = add(Name1),
  2 = get_tablesize(),
  {error, already_exists} = add(Name1),
  2 = get_tablesize(),
  ok = add(Name2, MWL),
  4 = get_tablesize(),
  {ok, _} = get_routing(Name1),
  {ok, _} = get_middleware_list(Name1),
  {ok, _} = get_routing(Name2),
  {ok, killed} = kill(Name1),
  ok = ensure_tablesize(2, 5000),
  {error, not_found} = get_routing(Name1),
  {ok, shutting_down} = shutdown(Name2),
  ok = ensure_tablesize(0, 5000),
  {error, not_found} = get_routing(Name2),
  ok = set_autocreate(true),
  {ok, _} = get_routing(Name1),
  ok = set_autocreate(false),
  2 = get_tablesize(),
  {ok, killed} = kill(Name1),
  ok = ensure_tablesize(0, 5000),
  timeout = ensure_tablesize(5, 10),
  {ok, stopped} = stop().


get_tablesize() ->
  Pid = whereis(?MODULE),
  Tables = ets:all(),
  [Table] = lists:filter(fun(T) -> ets:info(T, owner) == Pid end, Tables),
  ets:info(Table, size).


ensure_tablesize(_Number, MaxTime) when MaxTime =< 0 ->
  timeout;
ensure_tablesize(Number, MaxTime) ->
  case get_tablesize() of
    Number -> ok;
    _ ->
      receive
      after 10 -> ok
      end,
      NewTime = MaxTime - 10,
      ensure_tablesize(Number, NewTime)
  end.
