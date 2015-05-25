%%
%% Copyright (c) 2015 Bas Wegh
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

-module(callee_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests.
-export([api_call/1]).

-define(REALM,<<"test2">>).

%% ct.
all() ->
	[api_call].

init_per_suite(Config) ->
  {ok,_} = application:ensure_all_started(awre),
  {ok,_} = application:ensure_all_started(ranch),
  {ok,_} = application:ensure_all_started(sasl),
  {ok,_} = application:ensure_all_started(erwa),
  ok = erwa:start_realm(?REALM),
  Config.

end_per_suite(Config) ->
  {ok,_} = erwa:stop_realm(?REALM),
  Config.


api_call(_) ->
  SubList = <<"wamp.subscription.list">>,
  RegList = <<"wamp.registration.list">>,
  SessionCount = <<"wamp.session.count">>,
  SessionList = <<"wamp.session.list">>,

  {ok,Con} = awre:start_client(),
  {ok,_SessionId,_RouterDetails} = awre:connect(Con,?REALM),

  {ok,_,SubResult,undefined} = awre:call(Con,#{},SubList),
  [#{exact := [], wildcard := [], prefix := []}] = SubResult,

  {ok,_,RegResult,undefined} = awre:call(Con,#{},RegList),
  [#{exact := [], wildcard := [], prefix := []}] = RegResult,

  {ok,_,CountResult,undefined} = awre:call(Con,#{},SessionCount),
  [1] = CountResult,

  {ok,_,Ids,undefined} = awre:call(Con,#{},SessionList),
  1 = length(Ids),
  ok = awre:stop_client(Con),
  ok.



