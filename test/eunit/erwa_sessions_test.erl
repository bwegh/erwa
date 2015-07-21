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
-module(erwa_sessions_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

stat_stop_test() ->
  {ok, _} = erwa_sessions:start(),
  {ok, stopped} = erwa_sessions:stop().

simple_test() ->
  {ok, _} = erwa_sessions:start(),
  0 = get_tablesize(),
  {ok, _} = erwa_sessions:register_session(<<"test_realm">>),
  2 = get_tablesize(),
  ok = erwa_sessions:unregister_session(),
  0 = get_tablesize(),
  {ok, stopped} = erwa_sessions:stop().

die_test() ->
  {ok, _} = erwa_sessions:start(),
  0 = get_tablesize(),
  F =
    fun() ->
      erwa_sessions:register_session(<<"test_realm">>),
      timer:sleep(200)
    end,
  spawn(F),
  ok = ensure_tablesize(2, 500),
  ok = ensure_tablesize(0, 5000),
  {ok, stopped} = erwa_sessions:stop().

garbage_test() ->
  {ok, Pid} = erwa_sessions:start(),
  ignored = gen_server:call(erwa_sessions, some_garbage),
  ok = gen_server:cast(erwa_sessions, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = erwa_sessions:stop().


%% @private
get_tablesize() ->
  Pid = whereis(erwa_sessions),
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