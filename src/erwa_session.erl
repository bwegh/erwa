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

-module(erwa_session).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([set_id/2]).
-endif.

-export([create/0]).
-export([
        set_peer/2,
        set_ssl/2,
        set_source/2
        ]).
-export([
        get_peer/1,
        get_ssl/1,
        get_source/1,
        get_id/1
        ]).
-export([handle_message/2]).
-export([handle_info/2]).
-export([check_out_message/2]).


-record(state, {
                id = none,
                is_auth = false,
                realm_name = none,
                mwl = [],
                version = none,

                routing_pid = none,
                broker = none,
                dealer = none,

                source = unknown,
                peer = unknown,
                ssl = false,

                goodbye_sent = false,

                calls = [],

                invocation_id = 1,
                invocations = []
                }).


%
% Session Scope IDs
%
%     ERROR.Request
%     PUBLISH.Request
%     PUBLISHED.Request
%     SUBSCRIBE.Request
%     SUBSCRIBED.Request
%     UNSUBSCRIBE.Request
%     UNSUBSCRIBED.Request
%     CALL.Request
%     CANCEL.Request
%     RESULT.Request
%     REGISTER.Request
%     REGISTERED.Request
%     UNREGISTER.Request
%     UNREGISTERED.Request
%     INVOCATION.Request
%     INTERRUPT.Request
%     YIELD.Request
%
% IDs in the session scope SHOULD be incremented by 1 beginning with 1
% (for each direction - Client-to-Router and Router-to-Client)
%


create() ->
  Version = erwa:get_version(),
  #state{version=Version}.

set_peer(Peer,State) ->
  State#state{peer=Peer}.

set_source(Source,State) ->
  State#state{source=Source}.

set_ssl(SSL,State) ->
  State#state{ssl=SSL}.

get_peer(#state{peer=Peer}) ->
  Peer.

get_source(#state{source=Source}) ->
  Source.

get_ssl(#state{ssl=SSL}) ->
  SSL.

get_id(#state{id=ID}) ->
  ID.

-spec handle_message( term() , record(state)) ->
  { ok, record(state) } |
  {stop, record(state)} |
  {reply, Message:: term(), record(state)} |
  {reply_stop, Message:: term(), record(state) }.


handle_message(InMsg,State) ->
  case hndl_msg(InMsg,State) of
    {Result, OutMsg, State1} ->
      {ok, Msg, State2} = check_out_message(OutMsg, State1),
      {Result, Msg, State2};
    Other -> Other
  end.

-spec handle_info( term() , record(state)) ->
  { ok, record(state) } |
  {stop, record(state)} |
  {send, Message:: term(), record(state)} |
  {send_stop, Message:: term(), record(state) }.

handle_info(Info,State) ->
  case hndl_info(Info,State) of
    {Result, OutMsg, State1} ->
      {ok, Msg, State2} = check_out_message(OutMsg, State1),
      {Result, Msg, State2};
    Other -> Other
  end.


-spec check_out_message(term(), record(state) ) -> {ok, term(), record(state)}.
check_out_message(Msg,State) ->
  %% @todo: implement
  {ok, Msg, State}.



hndl_msg({hello,RealmName,_Details},#state{version=Version}=State) ->
  % Todo: middleware check
  MWResult = true,
  case {MWResult,erwa_realms:get_routing(RealmName)} of
    {true,{ok, RoutingPid}} ->
      {ok, SessionId} = erwa_sessions:register_session(),
      ok = erwa_routing:connect(RoutingPid),
      {ok,Broker} = erwa_routing:get_broker(RoutingPid),
      {ok,Dealer} = erwa_routing:get_dealer(RoutingPid),
      {ok,MWL} = erwa_realms:get_middleware_list(RealmName),
      BrokerFeat = erwa_broker:get_features(Broker),
      DealerFeat = erwa_dealer:get_features(Dealer),
      State1 = State#state{id=SessionId,mwl=MWL,realm_name=RealmName, routing_pid=RoutingPid, is_auth=true, dealer=Dealer, broker=Broker},
      Msg ={welcome,SessionId,[{agent,Version},{roles,[BrokerFeat,DealerFeat]}]},
      {reply,Msg,State1};
    {_,{error,not_found}} ->
      Msg = {abort,[],no_such_realm},
      {reply_stop,Msg,State};
    {false, _ } ->
      Msg = {abort,[],not_authorized},
      {reply_stop,Msg,State}
  end;

hndl_msg({authenticate,_Signature,_Extra},#state{}=State) ->
%TODO: implement
  {ok,State};

hndl_msg({goodbye,_Details,_Reason},#state{goodbye_sent=GBSent}=State) ->
  ok = close_session(State),
  case GBSent of
    true ->
      {stop,#state{}};
    false ->
      Msg = {goodbye,[],goodbye_and_out},
      {reply_stop,Msg,#state{}}
  end;

hndl_msg(Msg,#state{is_auth=true}=State) ->
  hndl_msg_authed(Msg,State);

hndl_msg(_Msg,_State) ->
  erwa_sessions:unregister_session(),
  {stop,#state{}}.




hndl_msg_authed({subscribe,RequestId,Options,Topic},#state{broker=Broker}=State) ->
  {ok,SubscriptionId} = erwa_broker:subscribe(Topic,Options,State,Broker),
  {reply, {subscribed,RequestId,SubscriptionId}, State };

hndl_msg_authed({unsubscribe,RequestId,SubscriptionId},#state{broker=Broker}=State) ->
  ok = erwa_broker:unsubscribe(SubscriptionId,Broker),
  {reply, {unsubscribed,RequestId},State};

hndl_msg_authed({publish,RequestId,Options,Topic,Arguments,ArgumentsKw},#state{broker=Broker}=State) ->
  {ok,PublicationId} = erwa_broker:publish(Topic,Options,Arguments,ArgumentsKw,State,Broker),
  case lists:keyfind(acknowledge,1,Options) of
    {acknowledge,true} ->
      {reply,{published,RequestId,PublicationId},State};
    _ ->
      {ok,State}
  end;



hndl_msg_authed({register,RequestId,Options,ProcedureUri},#state{dealer=Dealer}=State) ->
  {ok,RegistrationId} = erwa_dealer:register(ProcedureUri,Options,State,Dealer),
  {reply,{registered,RequestId,RegistrationId},State};

hndl_msg_authed({unregister,RequestId,RegistrationId},#state{dealer=Dealer} = State) ->
  ok = erwa_dealer:unregister(RegistrationId,Dealer),
  {reply,{unregistered,RequestId} ,State};

hndl_msg_authed({call,RequestId,Options,ProcedureUri,Arguments,ArgumentsKw},#state{dealer=Dealer,calls=Calls}=State) ->
  case erwa_dealer:call(ProcedureUri,RequestId,Options,Arguments,ArgumentsKw,Dealer) of
    {ok,Pid} ->
      {ok,State#state{calls=[{RequestId,Pid}|Calls]}};
    {error,procedure_not_found} ->
      {reply, {error,call,RequestId,[],no_such_procedure}, State};
    {error,_} ->
      %% TODO: change the reply for eg partitioned calls
      {reply, {error,call,RequestId,[],no_such_procedure}, State}
  end;

hndl_msg_authed({error,invocation,InvocationId,Details,Error,Arguments,ArgumentsKw},#state{invocations=Invs}=State) ->
  case lists:keyfind(InvocationId,1,Invs) of
    {InvocationId,Pid} ->
      ok = erwa_invocation:error(Pid,Details,Error,Arguments,ArgumentsKw),
      {ok,State#state{invocations = lists:keydelete(InvocationId,1,Invs)}};
    _ ->
      {ok,State}
  end;

hndl_msg_authed({yield,InvocationId,Options,Arguments,ArgumentsKw},#state{invocations=Invs}=State) ->
  case lists:keyfind(InvocationId,1,Invs) of
    {InvocationId,Pid} ->
      ok = erwa_invocation:yield(Pid,Options,Arguments,ArgumentsKw),
      {ok,State#state{invocations = lists:keydelete(InvocationId,1,Invs)}};
    _ ->
      {ok,State}
  end;

hndl_msg_authed(_Msg,_State) ->
  erwa_sessions:unregister_session(),
  {stop, #state{} }.






hndl_info({invocation,set_request_id,ProcedureId,Options,Arguments,ArgumentsKw},
          #state{invocation_id=ID, invocations=Invs}=State) ->
  {invocation_pid,Pid} = lists:keyfind(invocation_pid,1,Options),
  Options1 = lists:keydelete(invocation_pid,1,Options),
  NewState = State#state{invocations=[{ID,Pid}|Invs],invocation_id=ID+1},
  {send,{invocation,ID,ProcedureId,Options1,Arguments,ArgumentsKw},NewState};

hndl_info({result,CallRequestId,_,_,_}=Msg,#state{calls=Calls}=State) ->
  {send,Msg,State#state{calls=lists:keydelete(CallRequestId,1,Calls)}};

hndl_info({error,call,CallRequestId,_,_,_,_}=Msg,#state{calls=Calls}=State) ->
  {send,Msg,State#state{calls=lists:keydelete(CallRequestId,1,Calls)}};

hndl_info({event,_,_,_,_,_}=Msg,State) ->
  {send,Msg,State};

hndl_info(routing_closing,#state{goodbye_sent=GBsent}=State) ->
  Msg = {goodbye,[],close_realm},
  case GBsent of
    false ->
      {send,Msg,State#state{goodbye_sent=true}};
    _ ->
      ok = close_session(State),
      {stop,State}
  end;

hndl_info(shutdown,State) ->
  {stop,State};

hndl_info(Info, State) ->
  error("unknown Msg ~p~n",[Info]),
  {ok,State}.



close_session(#state{broker=Broker,dealer=Dealer,routing_pid=RoutingPid}) ->
  ok = erwa_broker:unsubscribe_all(Broker),
  ok = erwa_dealer:unregister_all(Dealer),
  ok = erwa_routing:disconnect(RoutingPid),
  ok = erwa_sessions:unregister_session(),
  ok.

-ifdef(TEST).

set_id(Id,State) ->
  State#state{id=Id}.


-endif.
