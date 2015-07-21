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
-module(erwa_publications_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

get_tablesize() ->
  Pid = whereis(erwa_publications),
  Tables = ets:all(),
  [Table] = lists:filter(fun(T) -> ets:info(T, owner) == Pid end, Tables),
  ets:info(Table, size).

stat_stop_test() ->
  {ok, _} = erwa_publications:start(),
  {ok, stopped} = erwa_publications:stop().

simple_test() ->
  {ok, _} = erwa_publications:start(),
  0 = get_tablesize(),
  {ok, _} = erwa_publications:get_pub_id(),
  1 = get_tablesize(),
  {ok, _} = erwa_publications:get_pub_id(),
  2 = get_tablesize(),
  {ok, stopped} = erwa_publications:stop().


garbage_test() ->
  {ok, Pid} = erwa_publications:start(),
  ignored = gen_server:call(erwa_publications, some_garbage),
  ok = gen_server:cast(erwa_publications, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = erwa_publications:stop().
