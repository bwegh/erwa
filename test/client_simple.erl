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

-module(client_simple).

-export([init/1]).
-export([on_connect/2]).
-export([on_test_event/6]).
-export([rpc_sum/5]).
-export([get_event_url/0]).
-export([get_rpc_url/0]).
-export([on_result/6]).
-export([all_done/1]).

-record(state,{
  subscription = undefined,
  event_received = false,
  registration = undefined,
  been_called = false
              }).

init(_) ->
  {ok,#state{}}.

on_connect(State,Con) ->
  {ok,SubscriptionId} = erwa_con:subscribe(Con,[{}],get_event_url(),on_test_event),
  {ok,RegistrationId} = erwa_con:register(Con,[{}],get_rpc_url(),rpc_sum),
  {ok,State#state{subscription=SubscriptionId, registration=RegistrationId}}.


on_test_event(_PublishId,_Details,_Arguments,_ArgumentsKw,#state{subscription=S}=State,Con) ->
  ok = erwa_con:unsubscribe(Con,S),
  {ok,State#state{event_received=true, subscription=undefined}}.

rpc_sum(_Details,[A,B],_ArgumentsKw,#state{registration=R}=State,Con) ->
  ok = erwa_con:unregister(Con,R),
  {ok,[{}],[A+B],undefined,State#state{been_called=true, registration=undefined}}.


get_event_url() ->
  <<"com.test.simple">>.

get_rpc_url() ->
  <<"com.test.sum">>.

on_result(_RequestId,_Details,_Results,_ResultsKw,State,_Connection) ->
  {ok,State}.


all_done(#state{event_received=true,been_called=true}) ->
  true;
all_done(_) ->
  false.

