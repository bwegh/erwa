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
-behaviour(gen_server).


-export([get_event_url/0]).
-export([get_rpc_url/0]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state,{
  con = undefined,
  session = undefined,
  subscription = undefined,
  event_received = false,
  registration = undefined,
  been_called = false,
  disconnected = false
              }).

init(Realm) ->
  {ok,Con} = erwa:start_client(),
  {ok,SessionId,_RouterDetails} = erwa:connect(Con,Realm),
  {ok,SubscriptionId} = erwa:subscribe(Con,[{}],get_event_url()),
  {ok,RegistrationId} = erwa:register(Con,[{}],get_rpc_url()),
  {ok,#state{con=Con,session=SessionId,subscription=SubscriptionId,registration=RegistrationId}}.

handle_call({all_done},_From,State) ->
  {reply,all_done(State),State}.

handle_cast(_Msg,State) ->
  {noreply,State}.


handle_info({erwa,{event,SubscriptionId,_PublicationId,_Details,_Arguments,_ArgumentsKw}},#state{subscription=SubscriptionId}=State) ->
  {noreply,State#state{event_received=true}};

handle_info({erwa,{invocation,RequestId,RegistrationId,_Details,[A,B],_ArgumentsKw}},#state{registration=RegistrationId,con=Con}=State) ->
  ok = erwa:yield(Con,RequestId,[{}],[A+B]),
  ok = erwa:stop_client(Con),
  {noreply,State#state{been_called=true,disconnected=true}};
handle_info(Msg,State) ->
  io:format("received message: ~p~n",[Msg]),
  {noreply,State}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.



get_event_url() ->
  <<"com.test.simple">>.

get_rpc_url() ->
  <<"com.test.sum">>.

all_done(#state{event_received=true,been_called=true,disconnected=true}) ->
  true;
all_done(_) ->
  false.
