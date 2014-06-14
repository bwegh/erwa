%%
%% Copyright (c) 2014 Bas Wegh
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

-module(roundtrip_client2).
-behaviour(erwa_client).

-export([init/1]).
-export([on_connect/2]).
-export([on_result/6]).

-export([on_test_event/6]).
-export([rpc_diff/5]).

-export([test_passed/1]).


-record(state,{
  event_url = undefined,
  rpc_url = undefined,
  subscription = undefined,
  event_received = false,
  registration = undefined,
  been_called = false,
  call_id = undefined,
  result_received = false
              }).

init(Args) ->
  {event_url,Event} = lists:keyfind(event_url,1,Args),
  {rpc_url,RPC} = lists:keyfind(rpc_url,1,Args),
  {ok,#state{event_url=Event, rpc_url=RPC}}.

on_connect(#state{event_url=E, rpc_url=R}=State,Con) ->
  {ok,SubscriptionId} = erwa_con:subscribe(Con,[{}],E,on_test_event),
  {ok,RegistrationId} = erwa_con:register(Con,[{}],R,rpc_diff),
  {ok,_Reply} = erwa_con:publish(Con,[{}],E),
  {ok,State#state{subscription=SubscriptionId, registration=RegistrationId}}.

on_result(RequestId,_Details,[14],_ResultsKw,#state{call_id=RequestId}=State,_Con) ->
  {ok,State#state{result_received=true}}.

on_test_event(_PubId, _Details, _Arguments, _ArgumentsKW, #state{subscription=Id}=State, Con) ->
  ok = erwa_con:unsubscribe(Con,Id),
  {ok,State#state{event_received=true}}.

rpc_diff(_Details,[A,B],_ArgumentsKw,#state{registration=Id}=State,Con) ->
  {ok,RequestId} = erwa_con:call(Con,[{}],<<"com.test.sum">>,[9,5]),
  ok = erwa_con:unregister(Con,Id),
  {ok,[{}],[A-B],undefined,State#state{been_called=true,call_id=RequestId}}.


test_passed(#state{event_received=true,been_called=true,result_received=true}) ->
  true;
test_passed(_) ->
  false.
