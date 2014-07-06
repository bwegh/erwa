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

%% @private
-module(erwa_con).
-behaviour(gen_server).

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
-define(CLIENT_ROLES,{<<"roles">>,[{<<"publisher">>,[{}]},{<<"subscriber">>,[{}]},{<<"caller">>,[{}]},{<<"callee">>,[{}]}]}).


-record(state,{
    realm = unknown,
    router=undefined,
    socket=undefined,
    buffer = <<"">>,
    sess = undefined,
    ets = undefined,
    enc = undefined
  }).

-record(subscription,{
  id = undefined,
  mfa = undefined,
  pid=undefined}).

-record(ref, {
  req = undefined,
  method = undefined,
  ref=undefined,
  args = []
              }).

-record(registration,{
  id = undefined,
  mfa = undefined,
  pid = undefined
                      }).

%-record(call,{
%  id = undefined,
%  mfa = undefined}).



start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).



-spec init(Params :: list() ) -> {ok,#state{}}.
init([]) ->
  Ets = ets:new(con_data,[bag,protected,{keypos,2}]),
  {ok,#state{ets=Ets}}.



-spec handle_call(Msg :: term(), From :: term(), #state{}) -> {reply,Msg :: term(), #state{}}.
handle_call({connect,Host,Port,Realm,Encoding},From,#state{ets=Ets}=State) ->
  Enc = case Encoding of
          json -> raw_json;
          raw_json -> raw_json;
          msgpack -> raw_msgpack;
          _ -> raw_msgpack
        end,
  {R,S} =
    case Host of
      undefined ->
        {ok, Router} =  erwa:get_router_for_realm(Realm),
        {Router,undefined};
      _ ->
        {ok, Socket} = gen_tcp:connect(Host,Port,[binary,{packet,0}]),
        {undefined,Socket}
    end,
  State1 = State#state{enc=Enc,router=R,socket=S,realm=Realm},
  ok = raw_send({hello,Realm,[?CLIENT_ROLES]},State1),
  true = ets:insert_new(Ets,#ref{req=hello,method=hello,ref=From}),
  {noreply,State1};

handle_call({subscribe,Options,Topic,Mfa},From,State) ->
  send({subscribe,request_id,Options,Topic},From,[{mfa,Mfa}],State),
  {noreply,State};

handle_call({unsubscribe,SubscriptionId},From,State) ->
  send({unsubscribe,request_id,SubscriptionId},From,[{sub_id,SubscriptionId}],State),
  {noreply,State};

handle_call({publish,Options,Topic,Arguments,ArgumentsKw},From,State) ->
  send({publish,request_id,Options,Topic,Arguments,ArgumentsKw},From,[],State),
  {reply,ok,State};

handle_call({register,Options,Procedure,Mfa},From,State) ->
  send({register,request_id,Options,Procedure},From,[{mfa,Mfa}],State),
  {noreply,State};

handle_call({unregister,RegistrationId},From,State) ->
  send({unregister,request_id,RegistrationId},From,[{reg_id,RegistrationId}],State),
  {noreply,State};

handle_call({call,Options,Procedure,Arguments,ArgumentsKw},From,State) ->
  ok = send({call,request_id,Options,Procedure,Arguments,ArgumentsKw},From,[],State),
  {noreply,State};

handle_call({yield,_,_,_,_}=Msg,_From,State) ->
  ok = raw_send(Msg,State),
  {reply,ok,State};

handle_call(_Msg,_From,State) ->
  {noreply,State}.



handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp,Socket,Data},#state{buffer=Buffer,socket=Socket,enc=Enc}=State) ->
  {Messages,NewBuffer} = erwa_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
  handle_messages(Messages,State),
  {noreply,State#state{buffer=NewBuffer}};
handle_info({erwa,Msg}, State) ->
  handle_message(Msg,State),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.




handle_message({welcome,SessionId,RouterDetails},#state{ets=Ets}) ->
  [#ref{method=hello,ref=From}] = ets:lookup(Ets,hello),
  ets:delete(Ets,welcome),
  gen_server:reply(From,{ok,SessionId,RouterDetails});

%handle_message({abort,},#state{ets=Ets}) ->

%handle_message({goodbye,},#state{ets=Ets}) ->

%handle_message({error,},#state{ets=Ets}) ->

%handle_message({published,},#state{ets=Ets}) ->

handle_message({subscribed,RequestId,SubscriptionId},#state{ets=Ets}) ->
  [#ref{method=subscribe,ref=From,args=Args}] = ets:lookup(Ets,RequestId),
  ets:delete(Ets,RequestId),
  {mfa,Mfa} = lists:keyfind(mfa,1,Args),
  {Pid,_} = From,
  ets:insert_new(Ets,#subscription{id=SubscriptionId,mfa=Mfa,pid=Pid}),
  gen_server:reply(From,{ok,SubscriptionId});

handle_message({unsubscribed,RequestId},#state{ets=Ets}) ->
  [#ref{method=unsubscribe,ref=From,args=Args}] = ets:lookup(Ets,RequestId),
  ets:delete(Ets,RequestId),
  SubscriptionId = lists:keyfind(sub_id,1,Args),
  ets:delete(Ets,SubscriptionId),
  gen_server:reply(From,ok);

handle_message({event,SubscriptionId,_PublicationId,Details,Arguments,ArgumentsKw}=Msg,#state{ets=Ets}) ->
  [#subscription{
                id = SubscriptionId,
                mfa = Mfa,
                pid=Pid}] = ets:lookup(Ets,SubscriptionId),
  case Mfa of
    undefined ->
      Pid ! {erwa,Msg};
    {M,F,S}  ->
      erlang:apply(M,F,[Details,Arguments,ArgumentsKw,S])
  end;
handle_message({result,RequestId,Details,Arguments,ArgumentsKw},#state{ets=Ets}) ->
  [#ref{method=call,ref=From}] = ets:lookup(Ets,RequestId),
  ets:delete(Ets,RequestId),
  gen_server:reply(From,{ok,Details,Arguments,ArgumentsKw});

handle_message({registered,RequestId,RegistrationId},#state{ets=Ets}) ->
  [#ref{method=register,ref=From,args=Args}] = ets:lookup(Ets,RequestId),
  ets:delete(Ets,RequestId),
  {mfa,Mfa} = lists:keyfind(mfa,1,Args),
  {Pid,_} = From,
  ets:insert_new(Ets,#registration{id=RegistrationId,mfa=Mfa,pid=Pid}),
  gen_server:reply(From,{ok,RegistrationId});

handle_message({unregistered,RequestId},#state{ets=Ets}) ->
  [#ref{method=unregister,ref=From,args=Args}] = ets:lookup(Ets,RequestId),
  ets:delete(Ets,RequestId),
  RegistrationId = lists:keyfind(reg_id,1,Args),
  ets:delete(Ets,RegistrationId),
  gen_server:reply(From,ok);

handle_message({invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}=Msg,#state{ets=Ets}=State) ->
  [#registration{
                id = RegistrationId,
                mfa = Mfa,
                pid=Pid}] = ets:lookup(Ets,RegistrationId),
  case Mfa of
    undefined ->
      Pid ! {erwa,Msg};
    {M,F,S}  ->
       {ok,Options,ResA,ResAKw} = erlang:apply(M,F,[Details,Arguments,ArgumentsKw,S]),
       ok = raw_send({yield,RequestId,Options,ResA,ResAKw},State)
  end;

handle_message({error,call,RequestId,Details,Error,Arguments,ArgumentsKw},#state{ets=Ets}) ->
  [#ref{method=call,ref=From}] = ets:lookup(Ets,RequestId),
  ets:delete(Ets,RequestId),
  gen_server:reply(From,{error,Details,Error,Arguments,ArgumentsKw});

handle_message(Msg,_State) ->
  io:format("unhandled message ~p~n",[Msg]).




handle_messages([],_State) ->
  ok;
handle_messages([Message|Messages],State) ->
  handle_message(Message,State),
  handle_messages(Messages,State).



send(Msg,From,Args,#state{ets=Ets}=State) ->
  RequestId = gen_id(State),
  Message = setelement(2,Msg,RequestId),
  Method = element(1,Message),
  true = ets:insert_new(Ets,#ref{req=RequestId,method=Method,ref=From,args=Args}),
  raw_send(Message,State).


raw_send(Message,#state{router=R,socket=S,enc=Enc}=State) ->
  case destination(State) of
    local ->
      ok = erwa_router:handle_wamp(R,Message);
    remote ->
      ok = gen_tcp:send(S,erwa_protocol:serialize(Message,Enc))
  end.

-spec destination(#state{}) -> local | remote.
destination(#state{socket=S}) ->
  case S of
    undefined ->
      local;
    _ ->
      remote
  end.

-spec gen_id(#state{}) -> non_neg_integer().
gen_id(#state{ets=Ets}=State) ->
  Id = crypto:rand_uniform(0,9007199254740992),
  case ets:lookup(Ets,Id) of
    [] ->
      Id;
    _ ->
      gen_id(State)
  end.





