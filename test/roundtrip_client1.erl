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

-module(roundtrip_client1).

-record(state,{
  con = undefined,
  session = undefined,
  subscription = undefined,
  event_received = false,
  registration = undefined,
  been_called = false,
  event_url = undefined,
  remote_rpc = undefined
              }).

-behaviour(gen_server).

-export([test_passed/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



init(Args) ->
  {event_url,Event} = lists:keyfind(event_url,1,Args),
  {rpc_url,RPC} = lists:keyfind(rpc_url,1,Args),
  {remote_rpc,RemoteRpc} = lists:keyfind(remote_rpc,1,Args),
  {realm,Realm} = lists:keyfind(realm,1,Args),
  Enc =
    case lists:keyfind(enc,1,Args) of
      {enc,E} -> E;
      _ -> json
    end,
  {ok,Con} = awre:start_client(),
  {ok,SessionId,_RouterDetails} =
    case lists:keyfind(tcp,1,Args) of
      {tcp,true} ->
        awre:connect(Con,"localhost",5555,Realm,Enc);
       _ ->
        awre:connect(Con,Realm)
    end,
  {ok,SubscriptionId} = awre:subscribe(Con,[{}],Event),
  {ok,RegistrationId} = awre:register(Con,[{}],RPC),
  ct:log("init of ~p done~n",[?MODULE]),
  {ok,#state{con=Con,session=SessionId,subscription=SubscriptionId,registration=RegistrationId,event_url=Event,remote_rpc=RemoteRpc}}.

handle_call({test_passed},_From,State) ->
  {reply,test_passed(State),State}.

handle_cast(_Msg,State) ->
  {noreply,State}.


handle_info({erwa,{event,SubscriptionId,_PublicationId,_Details,_Arguments,_ArgumentsKw}},#state{con=Con,subscription=SubscriptionId,remote_rpc=RPC}=State) ->
  ct:log("event at ~p~n",[?MODULE]),
  {ok,_Details,[4],_} = awre:call(Con,[{}],RPC,[9,5]),
  ok = awre:unsubscribe(Con,SubscriptionId),
  {noreply,State#state{event_received=true}};

handle_info({erwa,{invocation,RequestId,RegistrationId,_Details,[A,B],_ArgumentsKw}},#state{registration=RegistrationId,event_url=EventUrl,con=Con}=State) ->
  ct:log("invocation of ~p~n",[?MODULE]),
  ok = awre:yield(Con,RequestId,[{}],[A+B]),
  ok = awre:publish(Con,[{}],EventUrl,[3,4]),
  awre:unregister(Con,RegistrationId),
  ok = awre:stop_client(Con),
  {noreply,State#state{been_called=true}};

handle_info(Msg,State) ->
  ct:log("~p received message: ~p (state: ~p)~n",[?MODULE,Msg,State]),
  {noreply,State}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.


test_passed(#state{event_received=true,been_called=true}) ->
  true;
test_passed(_) ->
  false.
