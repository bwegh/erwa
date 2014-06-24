%% do with this code what ever you like
%% Bas Wegh

-module(simple_client).
-behaviour(gen_server).

-define(RPC_SQUARE_URL,<<"ws.wamp.test.square">>).
-define(RPC_ECHO_URL,<<"ws.wamp.test.echo">>).
-define(EVENT_URL,<<"ws.wamp.test.info">>).
-define(REALM,<<"ws.wamp.test">>).
-define(HOST,"localhost"). % has to be a string
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
  rpc_echo_id = undefined,
  event_sub_id = undefined
              }).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init(_) ->
  io:format("starting client~n"),
  {ok,Con} = erwa:start_client(),
  {ok,SessionId,_RouterDetails} = erwa:connect(Con,?HOST,?PORT,?REALM,?ENCODING),
  {ok,SubId} = erwa:subscribe(Con,[{}],?EVENT_URL),
  {ok,EchoRPCId} = erwa:register(Con,[{}],?RPC_ECHO_URL),
  {ok,#state{con=Con,session=SessionId,rpc_echo_id=EchoRPCId,event_sub_id=SubId}}.

handle_call(_,_From,State) ->
  {noreply,State}.

handle_cast(_Msg,State) ->
  {noreply,State}.


handle_info({erwa,{event,SubId,_PublicationId,_Details,Arguments,ArgumentsKw}},#state{event_sub_id=SubId,con=Con}=State) ->
  io:format("received event ~p ~p~n",[Arguments,ArgumentsKw]),
  {ok,Details,ResA,ResAKw} = erwa:call(Con,[{}],?RPC_SQUARE_URL,[7]),
  [Res] = ResA,
  io:format("result of call was: ~B ~p [~p]~n",[Res,ResAKw,Details]),
  Res = 49,
  ok = erwa:unsubscribe(Con,SubId),
  {noreply,State};


handle_info({erwa,{invocation,RequestId,RpcId,_Details,Arguments,ArgumentsKw}},#state{rpc_echo_id=RpcId,con=Con}=State) ->
  %invocation of the echo rpc
  ok = erwa:yield(Con,RequestId,[{}],Arguments,ArgumentsKw),
  ok = erwa:unregister(Con,RpcId),
  {noreply,State};

handle_info(Msg,State) ->
  io:format("received message: ~p~n",[Msg]),
  {noreply,State}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

