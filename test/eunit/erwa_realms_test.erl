%%
%% Copyright (c) 2014-2015 Bas Wegh
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
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
  ok = erwa_realms_man:set_autocreate(false),
  0 = get_tablesize(),
  {error, not_found} = erwa_realms_man:get_routing(Name1),
  0 = get_tablesize(),
  {error, not_running} = erwa_realms_man:kill(Name1),
  0 = get_tablesize(),
  ok = erwa_realms_man:add(Name1),
  2 = get_tablesize(),
  {error, already_exists} = erwa_realms_man:add(Name1),
  2 = get_tablesize(),
  ok = erwa_realms_man:add(Name2, MWL),
  4 = get_tablesize(),
  {ok, _} = erwa_realms_man:get_routing(Name1),
  {ok, _} = erwa_realms_man:get_middleware_list(Name1),
  {ok, _} = erwa_realms_man:get_routing(Name2),
  {ok, killed} = erwa_realms_man:kill(Name1),
  ok = ensure_tablesize(2, 5000),
  {error, not_found} = erwa_realms_man:get_routing(Name1),
  {ok, shutting_down} = erwa_realms_man:shutdown(Name2),
  ok = ensure_tablesize(0, 5000),
  {error, not_found} = erwa_realms_man:get_routing(Name2),
  ok = erwa_realms_man:set_autocreate(true),
  {ok, _} = erwa_realms_man:get_routing(Name1),
  ok = erwa_realms_man:set_autocreate(false),
  2 = get_tablesize(),
  {ok, killed} = erwa_realms_man:kill(Name1),
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
