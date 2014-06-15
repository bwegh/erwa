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

-module(erwa_con).
-behaviour(gen_server).

-export([subscribe/4]).
-export([unsubscribe/2]).
-export([publish/3,publish/4,publish/5]).

-export([register/4]).
-export([unregister/2]).
-export([call/3,call/4,call/5]).

-export([get_client_state/1]).

%% API.
-export([start_link/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define(DEFAULT_PORT,5555).
-define(CLIENT_ROLES,[{<<"publisher">>,[{}]},{<<"subscriber">>,[{}]},{<<"caller">>,[{}]},{<<"callee">>,[{}]}]).


-record(state,{
    realm = unknown,
    module = undefined,
    router=undefined,
    socket=undefined,
    buffer = <<"">>,
    host = undefined,
    port = undefined,
    cs = undefined,
    sess = undefined,
    ets = undefined
  }).

%-record(subscription,{
%  id = undefined,
%  method = undefined}).

-record(registration,{
  id = undefined,
  method = undefined}).

-record(call,{
  id = undefined,
  mfa = undefined}).

subscribe(#state{module=Module}=State,Options,Topic,Method) ->
  true = erwa_client:is_valid_event(Module,Method),
  {ok,RequestId} = subscribe(destination(State),Options,Topic,Method,State),
  %true = ets:insert_new(Ets,#subscription{id=SubscriptionId,method=Method}),
  {ok,RequestId}.

subscribe(local,Options,Topic,_Method,#state{router=Router})->
  RequestId = gen_id(),
  Msg = erwa_router:subscribe(Router,RequestId,Options,Topic),
  self() ! {erwa,Msg},
  {ok,RequestId};
subscribe(remote,Options,Topic,_Method,#state{socket=Socket})->
  RequestId = gen_id(),
  gen_tcp:send(Socket,encode(erwa_protocol:to_wamp({subscribe,RequestId,Options,Topic}))),
  {ok,RequestId}.

unsubscribe(State,SubscriptionId) ->
  ok = unsubscribe(destination(State),SubscriptionId,State),
  ok.

unsubscribe(local,SubscriptionId,#state{router=Router,ets=Ets}) ->
  RequestId = gen_id(),
  {unsubscribed,RequestId} = erwa_router:unsubscribe(Router,RequestId,SubscriptionId),
  true = ets:delete(Ets,SubscriptionId),
  ok.



publish(State,Options,Topic) ->
  publish(State,Options,Topic,undefined,undefined).
publish(State,Options,Topic,Arguments)->
  publish(State,Options,Topic,Arguments,undefined).
publish(State,Options,Topic,Arguments,ArgumentsKw) ->
  {ok,Reply} = publish(destination(State),Options,Topic,Arguments,ArgumentsKw,State),
  {ok,Reply}.

publish(local,Options,Topic,Arguments,ArgumentsKw,#state{router=Router}) ->
  RequestId = gen_id(),
  erwa_router:publish(Router,RequestId,Options,Topic,Arguments,ArgumentsKw),
  {ok,noreply};
publish(remote,Options,Topic,Arguments,ArgumentsKw,#state{socket=Socket}) ->
  RequestId = gen_id(),
  gen_tcp:send(Socket,encode(erwa_protocol:to_wamp({publish,RequestId,Options,Topic,Arguments,ArgumentsKw}))),
  {ok,noreply}.

register(#state{module=Module,ets=Ets}=State,Options,Procedure,Method) ->
  true = erwa_client:is_valid_rpc(Module,Method),
  {ok,RegistrationId} = register(destination(State),Options,Procedure,Method,State),
  true = ets:insert_new(Ets,#registration{id=RegistrationId,method=Method}),
  {ok,RegistrationId}.

register(local,Options,Procedure,_Method,#state{router=Router}) ->
  RequestId = gen_id(),
  {registered,RequestId,RegistrationId} = erwa_router:register(Router,RequestId,Options,Procedure),
  {ok,RegistrationId}.

unregister(#state{ets=Ets}=State,RegistrationId) ->
  ok = unregister(destination(State),RegistrationId,State),
  true = ets:delete(Ets,RegistrationId),
  ok.

unregister(local,RegistrationId,#state{router=Router}) ->
  RequestId = gen_id(),
  {unregistered,RequestId} = erwa_router:unregister(Router,RequestId,RegistrationId),
  ok.


call(State,Options,ProcedureUrl) ->
  call(State,Options,ProcedureUrl,undefined,undefined).
call(State,Options,ProcedureUrl,Arguments) ->
  call(State,Options,ProcedureUrl,Arguments,undefined).
call(State,Options,ProcedureUrl,Arguments,ArgumentsKw) ->
  {ok,RequestId} = call(destination(State),Options,ProcedureUrl,Arguments,ArgumentsKw,State),
  {ok,RequestId}.

call(local,Options,ProcedureUrl,Arguments,ArgumentsKw,#state{ets=Ets,router=Router}=State) ->
  RequestId = gen_id(),
  case ets:insert_new(Ets,#call{id=RequestId}) of
    false ->
      call(local,Options,ProcedureUrl,Arguments,ArgumentsKw,State);
    true ->
      erwa_router:call(Router,RequestId,Options,ProcedureUrl,Arguments,ArgumentsKw),
      {ok,RequestId}
  end.




get_client_state(Con) ->
  {client_state,CS} = gen_server:call(Con,get_client_state),
  CS.




start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).



-spec init(Params :: list() ) -> {ok,#state{}}.
init(Args) ->
  {module, Module} = lists:keyfind(module,1,Args),
  {args, Arguments} = lists:keyfind(args,1,Args),
  true = erwa_client:is_valid_client_module(Module),
  {ok,ClientState} = Module:init(Arguments),
  {realm, Realm} = lists:keyfind(realm,1,Args),
  Host =
    case lists:keyfind(host,1,Args) of
      false -> undefined;
      H -> H
    end,
  Port =
    case {Host, lists:keyfind(port,1,Args)} of
      {undefined,_} -> undefined;
      {_,false} -> ?DEFAULT_PORT;
      {_,P} -> P
    end,
  Ets = ets:new(con_data,[bag,protected,{keypos,2}]),
  State1 = #state{realm=Realm, module=Module, host=Host, port=Port, ets=Ets,cs=ClientState},
  State2 = connect(destination(State1),State1),
  {ok,State2}.



-spec handle_call(Msg :: term(), From :: term(), #state{}) -> {reply,Msg :: term(), #state{}}.
handle_call(get_client_state,_From,#state{cs=ClientState}=State) ->
  {reply,{client_state,ClientState},State};
handle_call(_Msg,_From,State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({erwa,{welcome,SessionId,_Details}}, #state{module=Module,cs=ClientState}=State) ->
  State1 = State#state{sess=SessionId},
  {ok,NewClientState} = Module:on_connect(ClientState,State),
  {noreply,State1#state{cs=NewClientState}};
handle_info({erwa,{invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}}, #state{module=Module,ets=Ets,cs=ClientState}=State) ->
  case ets:match(Ets,{registration,RegistrationId,'$1'}) of
    [[Method]] ->
      {ok,Options,Result,ResultKw,NewClientState} = Module:Method(Details,Arguments,ArgumentsKw,ClientState,State),
      ok = yield(destination(State),RequestId,Options,Result,ResultKw,State),
      {noreply,State#state{cs=NewClientState}};
    _ ->
      %erwa_router:error()
      {noreply,State}
   end;
handle_info({erwa,{event,SubscriptionId,PublishId,Details,Arguments,ArgumentsKw}}, #state{module=Module,ets=Ets,cs=ClientState}=State) ->
  case ets:match(Ets,{subscription,SubscriptionId,'$1'}) of
    [[Method]] ->
      {ok,NewClientState} = Module:Method(PublishId,Details,Arguments,ArgumentsKw,ClientState,State),
      {noreply,State#state{cs=NewClientState}};
    [] ->
      {noreply,State}
  end;
handle_info({erwa,{result,RequestId,Details,Result,ResultKw}}, #state{module=Module,cs=ClientState}=State) ->
  {ok,NewClientState} = Module:on_result(RequestId,Details,Result,ResultKw,ClientState,State),
  {noreply, State#state{cs=NewClientState}};

handle_info({tcp,Socket,Data},#state{buffer=Buffer,socket=Socket}=State) ->
  Buf = <<Buffer/binary, Data/binary>>,
  NewBuffer = enqueue_messages_from_buffer(Buf),
  {noreply,State#state{buffer=NewBuffer}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.




connect(local,#state{realm=Realm}=State) ->
  {ok, Router} =  erwa:get_router_for_realm(Realm),
  Msg = erwa_router:hello(Router,[]),
  self() ! {erwa,Msg},
  State#state{router=Router};
connect(remote,#state{realm=Realm,host=Host,port=Port}=State) ->
  {ok, Socket} = gen_tcp:connect(Host,Port,[binary]),
  ok = gen_tcp:send(Socket,encode(erwa_protocol:to_wamp({hello,Realm,[{<<"roles">>,?CLIENT_ROLES}]}))),
  State#state{socket=Socket}.


yield(local,RequestId,Options,Result,ResultKw,#state{router=Router}) ->
  erwa_router:yield(Router,RequestId,Options,Result,ResultKw),
  ok.


-spec destination(#state{}) -> local | remote.
destination(#state{host=H}) ->
  case H of
    undefined ->
      local;
    _ ->
      remote
  end.

-spec gen_id() -> non_neg_integer().
gen_id() ->
  crypto:rand_uniform(0,9007199254740993).


enqueue_messages_from_buffer(<<Len:32/unsigned-integer-big,Data/binary>>=Buffer)  ->
  case byte_size(Data) >= Len of
    true ->
      <<Enc:Len/binary,NewBuffer/binary>> = Data,
      Wamp = decode(Enc),
      self() ! {erwa,erwa_protocol:to_erl(Wamp)},
      enqueue_messages_from_buffer(NewBuffer);
    false ->
      Buffer
  end.

decode(Message) ->
  decode(Message,msgpack).
decode(Message,json) ->
  jsx:decode(Message);
decode(Message,msgpack) ->
  {ok,Msg} = msgpack:unpack(Message,[jsx]),
  Msg.


encode(Message) ->
  encode(Message,msgpack).
encode(Message,json) ->
  Enc = jsx:encode(Message),
  Len = byte_size(Enc),
  <<Len:32/unsigned-integer-big,Enc/binary>>;
encode(Message,msgpack) ->
  Enc = msgpack:pack(Message,[jsx]),
  Len = byte_size(Enc),
  <<Len:32/unsigned-integer-big,Enc/binary>>.
