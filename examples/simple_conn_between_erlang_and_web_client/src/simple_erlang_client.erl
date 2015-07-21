
-module(simple_erlang_client).
-behaviour(gen_server).

-define(RPC_SQUARE_URL,<<"ws.wamp.test.square">>).
-define(RPC_ECHO_URL,<<"ws.wamp.test.echo">>).
-define(EVENT_URL,<<"ws.wamp.test.info">>).
%%-define(REALM,<<"ws.wamp.test">>).
-define(HOST,"localhost"). % has to be a string
-define(PORT,5555).
-define(ENCODING,msgpack). %% msgpack or json
-define(AWRE_OPTIONS, #{}).

-export([start_link/2]).

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
  rpc_echo_id = undefined,
  event_sub_id = undefined
              }).

start_link(Client, Realm) ->
	gen_server:start_link({local, Client}, ?MODULE, [Client, Realm], []). 
  	%%gen_server:start_link(?MODULE, [], []).

init([Client, Realm]) ->
  io:format("starting client ~p... ", [Client]),
  {ok,Con} = awre:start_client(),
  
  io:format("done.~nconnecting to realm ~p at ~p:~p ... ",[Realm,?HOST,?PORT]),
  {ok,SessionId,_RouterDetails} = awre:connect(Con,?HOST,?PORT,Realm,?ENCODING),
  
  io:format("done (~p).~nsubscribe to ~p ... ",[SessionId,?EVENT_URL]),
  {ok,SubId} = awre:subscribe(Con,?AWRE_OPTIONS,?EVENT_URL),
  io:format("subscribed (~p).~nregister ~p ... ",[SubId,?RPC_ECHO_URL]),
  
  {ok,EchoRPCId} = awre:register(Con,?AWRE_OPTIONS,?RPC_ECHO_URL),
  io:format("registered (~p).~nclient sucessfully initialized.~n",[EchoRPCId]),
  io:format("~nIf you send me an event on ~p I will call the procedure ~p~n",[?EVENT_URL,?RPC_SQUARE_URL]),
  
  {ok,#state{con=Con,session=SessionId,rpc_echo_id=EchoRPCId,event_sub_id=SubId}}.

handle_call(_,_From,State) ->
  {noreply,State}.

handle_cast(_Msg,State) ->
  {noreply,State}.


handle_info({awre,{event,SubId,_PublicationId,_Details,Arguments,ArgumentsKw}},#state{event_sub_id=SubId,con=Con}=State) ->
  io:format("received event ~p ~p on [~p]~n",[Arguments,ArgumentsKw,SubId]),
  Params = [3],
  io:format("calling ~p ~p ... ",[?RPC_SQUARE_URL,Params]),
  {ok,Details,ResA,ResAKw} = awre:call(Con,?AWRE_OPTIONS,?RPC_SQUARE_URL,Params),
  io:format("result is: ~p ~p [~p]~n",[ResA,ResAKw,Details]),
  ResA = [9],
%%   io:format("unsubscribing from ~p ... ",[SubId]),
%%   ok = awre:unsubscribe(Con,SubId),
%%   io:format("unsubscribed.~n"),
  {noreply,State};


handle_info({awre,{invocation,RequestId,RpcId,_Details,Arguments,ArgumentsKw}},#state{rpc_echo_id=RpcId,con=Con}=State) ->
  %invocation of the echo rpc
  io:format("been called [~p] with params ~p ~p ... will just send them back ...",[RpcId,Arguments,ArgumentsKw]),
  ok = awre:yield(Con,RequestId,?AWRE_OPTIONS,Arguments,ArgumentsKw),
%%   io:format("sent.~nunregistering ~p ... ",[RpcId]),
%%   ok = awre:unregister(Con,RpcId),
%%   io:format("unregistered.~n"),
  {noreply,State};

handle_info(Msg,State) ->
  io:format("~nreceived unknown message: ~p~n",[Msg]),
  {noreply,State}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

