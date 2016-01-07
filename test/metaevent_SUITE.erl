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


-module(metaevent_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests.
-export([session_meta/1]).

%% ct.
all() ->
	[
	session_meta
	].

-define(REALM,<<"meta_event">>).

init_per_suite(Config) ->
  {ok,_} = application:ensure_all_started(lager),
  {ok,_} = application:ensure_all_started(awre),
  {ok,_} = application:ensure_all_started(ranch),
  {ok,_} = application:ensure_all_started(sasl),
  {ok,_} = application:ensure_all_started(erwa),
  {ok,_} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_in_tcp, []),
  ok = erwa:start_realm(?REALM),
  Config.

end_per_suite(Config) ->
  {ok,_} = erwa:stop_realm(?REALM),
  ok = ranch:stop_listener(erwa_tcp),
  Config.


session_meta(_) ->
	Topic1 = <<"wamp.session.on_join">>,
	Topic2 = <<"wamp.session.on_leave">>,
  Client = fun() ->
             {ok,Con} = awre:start_client(),
             {ok,_SessionId,_RouterDetails} = awre:connect(Con,"localhost",5555,?REALM,json), 
             ok = receive
             after 100 ->
		ok
             end,
             ok = awre:stop_client(Con)
           end,

  {ok,Con} = awre:start_client(),
  {ok,_SessionId,_RouterDetails} = awre:connect(Con,?REALM ),
  {ok,SubscriptionId1} = awre:subscribe(Con,#{},Topic1),
  {ok,SubscriptionId2} = awre:subscribe(Con,#{},Topic2),
  spawn(Client),
  ok = receive
         {awre,{event,SubscriptionId1,_,_,_,_}} ->
           ok;
		Msg ->
			ct:log("received message ~p instead of 'on_join'~n",[Msg])
       after 500 ->
         timeout
       end,
  ok = receive
         {awre,{event,SubscriptionId2,_,_,_,_}} ->
	ok;
		M2 ->
			ct:log("received message ~p instead of 'on_leave'~n",[M2])
       after 500 ->
         timeout
       end,

  ok = awre:stop_client(Con),
  ok.



