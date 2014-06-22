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

-module(simple_client).
-behaviour(gen_server).

-define(RPC_SUM_URL,<<"ws.wamp.test.sum">>).
-define(RPC_ECHO_URL,<<"ws.wamp.test.echo">>).
-define(EVENT_URL,<<"ws.wamp.test.info">>).
-define(REALM,<<"ws.wamp.test">>).
-define(HOST,<<"localhost">>).
-define(PORT,5555).
-define(ENCODING,msgpack). %% msgpack or json

-export([start_link/0]).

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
  rpc_sum_id = undefined,
  rpc_echo_id = undefined,
  event_sub_id = undefined
              }).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init(_) ->
  {ok,Con} = erwa:start_client(),
  {ok,SessionId,_RouterDetails} = erwa:connect(Con,?HOST,?PORT,?REALM,?ENCODING),
  {ok,SubId} = erwa:subscribe(Con,[{}],?EVENT_URL),
  {ok,SumRPCId} = erwa:register(Con,[{}],?RPC_SUM_URL),
  {ok,EchoRPCId} = erwa:register(Con,[{}],?RPC_ECHO_URL),
  {ok,#state{con=Con,session=SessionId,rpc_sum_id=SumRPCId,rpc_echo_id=EchoRPCId,event_sub_id=SubId}}.

handle_call(_,_From,State) ->
  {noreply,State}.

handle_cast(_Msg,State) ->
  {noreply,State}.


handle_info({erwa,{event,SubId,_PublicationId,_Details,Arguments,ArgumentsKw}},#state{event_sub_id=SubId}=State) ->
  io:format("received event ~p ~p~n",[Arguments,ArgumentsKw]),
  {noreply,State};

handle_info({erwa,{invocation,RequestId,RpcId,_Details,[A,B],_ArgumentsKw}},#state{rpc_sum_id=RpcId,con=Con}=State) ->
  %invocation of the sum rpc
  ok = erwa:yield(Con,RequestId,[{}],[A+B]),
  {noreply,State};

handle_info({erwa,{invocation,RequestId,RpcId,_Details,Arguments,ArgumentsKw}},#state{rpc_echo_id=RpcId,con=Con}=State) ->
  %invocation of the echo rpc
  ok = erwa:yield(Con,RequestId,[{}],Arguments,ArgumentsKw),
  {noreply,State};

handle_info(Msg,State) ->
  io:format("received message: ~p~n",[Msg]),
  {noreply,State}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

