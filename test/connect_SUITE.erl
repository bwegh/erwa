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

-module(connect_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests.
-export([local_connect/1]).
-export([tcp_connect_msgpack/1]).
-export([tcp_connect_json/1]).
-export([tcp_subscribe/1]).
-export([tcp_publish/1]).
-export([tcp_register/1]).
-export([tcp_call/1]).

%% ct.
all() ->
	[tcp_connect_msgpack,tcp_connect_json,tcp_subscribe,tcp_publish,tcp_register,tcp_call].

-define(REALM,<<"test">>).

init_per_suite(Config) ->
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


local_connect(_) ->
  {ok,Con} = awre:start_client(),
  {ok,_,_} = awre:connect(Con,?REALM),
  ok = awre:stop_client(Con),
  ok.

tcp_connect_msgpack(_) ->
  {ok,Con} = awre:start_client(),
  {ok,_,_} = awre:connect(Con,"localhost",5555,?REALM,raw_msgpack),
  ok = awre:stop_client(Con),
  ok.

tcp_connect_json(_) ->
  {ok,Con} = awre:start_client(),
  {ok,_,_} = awre:connect(Con,"localhost",5555,?REALM,raw_json),
  ok = awre:stop_client(Con),
  ok.

tcp_subscribe(_) ->
  {ok,Con} = awre:start_client(),
  {ok,_,_} = awre:connect(Con,"localhost",5555,?REALM,raw_msgpack),
  Topic = <<"dev.erwa.event">>,
  {ok,SubscriptionId} = awre:subscribe(Con,#{},Topic),
  ok = awre:unsubscribe(Con,SubscriptionId),
  ok = awre:stop_client(Con),
  ok.


tcp_publish(_) ->
  Topic = <<"dev.erwa.event">>,
  MyPid = self(),
  Client = fun() ->
             {ok,Con} = awre:start_client(),
             {ok,_SessionId,_RouterDetails} = awre:connect(Con,"localhost",5555,?REALM,raw_msgpack),
             {ok,SubscriptionId} = awre:subscribe(Con,#{},Topic),
             MyPid ! ready_when_you_are,
             receive
               Msg ->
                 MyPid ! Msg
             after 500 ->
               MyPid ! no_message
             end,
             ok = awre:unsubscribe(Con,SubscriptionId),
             ok = awre:stop_client(Con),
             MyPid ! done
           end,

  spawn(Client),
  {ok,Con} = awre:start_client(),
  {ok,_SessionId,_RouterDetails} = awre:connect(Con,"localhost",5555,?REALM,raw_msgpack),
  {ok,_RPid} = erwa_realms:get_routing(?REALM),

  ok = receive
         ready_when_you_are ->
           ok
       after 1000 ->  timeout
       end,
  ok = awre:publish(Con,#{},Topic,[<<"test">>]),
  ok = receive
         {awre,{event,_,_,#{},[<<"test">>],undefined}} ->
           ok
       after 1000 ->
         timeout
       end,
  ok = awre:stop_client(Con),
  ok = receive
         done -> ok
       after 500 ->
         timeout
       end,
  ok.


tcp_register(_) ->
  {ok,Con} = awre:start_client(),
  {ok,_SessionId,_RouterDetails} = awre:connect(Con,"localhost",5555,?REALM,raw_msgpack),
  Topic = <<"dev.erwa.function">>,
  {ok,RegistrationId} = awre:register(Con,#{},Topic),
  ok = awre:unregister(Con,RegistrationId),
  ok = awre:stop_client(Con),
  ok.


tcp_call(_) ->
  Topic = <<"dev.erwa.sum">>,
  MyPid = self(),
  Client = fun() ->
             {ok,Con} = awre:start_client(),
             {ok,_SessionId,_RouterDetails} = awre:connect(Con,"localhost",5555,?REALM,raw_msgpack),
             {ok,RegistrationId} = awre:register(Con,#{},Topic),
             MyPid ! ready_when_you_are,
             {ok,RequestId,A,B} = receive
                                    {awre,{invocation,Req,_,_,[In1,In2],undefined}} ->
                                      {ok,Req,In1,In2}
                                  after 1000 ->  timeout
                                  end,
             ok = awre:yield(Con,RequestId,#{},[A+B]),
             ok = awre:unregister(Con,RegistrationId),
             ok = awre:stop_client(Con),
             MyPid ! done
           end,

  spawn(Client),
  {ok,Con} = awre:start_client(),
  {ok,_SessionId,_RouterDetails} = awre:connect(Con,"localhost",5555,?REALM,raw_msgpack),
  ok = receive
         ready_when_you_are -> ok
       after 1000 ->  timeout
       end,
  {ok,_,[9],_} = awre:call(Con,#{},Topic,[4,5]),
  ok = receive
         done -> ok
       after 1000 ->  timeout
       end,
  ok = awre:stop_client(Con),
  ok.



