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

-module(erwa_routing).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/0]).
-export([handle_message/2]).
-export([handle_info/2]).


-record(state, {
                id = none,
                is_auth = false,
                realm_name = none,
                goodbye_sent = false,
                will_pass = false,
                invocation_id = 1,
                invocations = [],
                calls = [],
                client_roles = none
                }).


-define(BROKER_FEATURES,#{features => #{
                            event_history => false,
                            partitioned_pubsub => false,
                            pattern_based_subscription => false,
                            publication_trustlevels => false,
                            publisher_exclusion => true,
                            publisher_identification => true,
                            subscriber_blackwhite_listing => true,
                            subscriber_list => false,
                            subscriber_metaevents => false
                           }

                         }
       ).

-define(DEALER_FEATURES,#{features => #{
                                 call_canceling =>             true,
                                 call_timeout =>               true,
                                 call_trustlevels =>           false,
                                 callee_blackwhite_listing =>  false,
                                 caller_exclusion =>           false,
                                 caller_identification =>      false,
                                 partitioned_rpc =>            false,
                                 pattern_based_registration => false,
                                 progressive_call_results =>   true,
                                 shared_registration =>         true
                                 }

                   }
        ).


-define(ROLES, #{ 
		  broker => ?BROKER_FEATURES,
		  dealer => ?DEALER_FEATURES
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


init() ->
  #state{}.

-spec handle_message( term() , #state{}) ->
  { ok, #state{} } |
  {stop, #state{}} |
  {reply, Message:: term(), #state{}} |
  {reply_stop, Message:: term(), #state{} }.


handle_message(InMsg,State) ->
  case hndl_msg(InMsg,State) of
    {Result, OutMsg, State1} ->
      case Result of
        reply_stop ->
          close_session(State1);
        _ -> ok
      end,
      check_out_message(Result, OutMsg, State1);
    {stop,NewState} ->
      close_session(NewState),
      {stop,NewState};
		Other -> Other
  end.

-spec handle_info( term() , #state{}) ->
  { ok, #state{} } |
  {stop, #state{}} |
  {send, Message:: term(), #state{}} |
  {send_stop, Message:: term(), #state{} }.

handle_info(Info,State) ->
  case hndl_info(Info,State) of
    {Result, OutMsg, State1} ->
      case Result of
        send_stop ->
          close_session(State1);
        _ -> ok
      end,
      check_out_message(Result, OutMsg, State1);
    {stop,NewState} ->
      close_session(NewState),
      {stop,NewState};
    Other -> Other
  end.


-spec check_out_message(atom(), term(), #state{} ) -> {atom(), term(),
                                                            #state{} } |
                                                           {ok, #state{} }.
check_out_message(Result, Msg,State) ->
  %% case erwa_middleware:validate_out_message(Msg,State) of
  %%   false ->
  %%     {ok,State};
  %%   OutMsg ->
  %%     {Result,OutMsg,State}
  %% end.
  {Result, Msg, State}.


hndl_msg({hello,RealmName,Details}, State) ->
  AuthId = maps:get(authid, Details, anonymous),
  Roles = maps:get(roles, Details, []),
  {ok, SessionId} = erwa_sess_man:create_session(),
  case {AuthId == anonymous, erwa_user_db:allow_anonymous(RealmName, tcp)} of 
    {true, true} -> 
      case erwa_sess_man:connect_to(RealmName) of
        ok ->
          %% SessionData = #{authid => anonymous, role => anonymous, session =>
          %%                 SessionId},
          WelcomeMsg ={welcome,SessionId,#{agent => erwa:get_version(), roles =>
										  ?ROLES}},
          {reply,WelcomeMsg,State#state{is_auth=true,
                                        realm_name=RealmName,
                                        client_roles=Roles,
                                        id=SessionId
                                        }}; 
        {error,_} ->
              erwa_sess_man:unregister_session(),
          {reply_stop, {abort, #{}, no_such_realm},State}
      end;
    {true, false} -> 
              erwa_sess_man:unregister_session(),
      {reply_stop, {abort, #{}, no_such_realm}, State};
    {false,_} ->
      AuthMethods = maps:get(authmethods, Details, []),
      authenticate(AuthMethods, RealmName, Details, State)
  end;

hndl_msg({authenticate,_Signature,_Extra}=Msg,#state{}=State) ->
  % case erwa_middleware:check_perm(Msg,State) of
  %  {true,_} ->
      #state{id=SessionId} = State,
      Msg ={welcome,SessionId,#{agent => erwa:get_version(), roles => ?ROLES }},
      {reply,Msg,State#state{is_auth=true}};
  %%   {false,Details} ->
  %%     OutDetails = maps:get(details,Details,#{}),
  %%     Error = maps:get(error,Details,#{}),
  %%     Msg = {abort,OutDetails,Error},
  %%     {reply_stop,Msg,State}
  %% end;

hndl_msg({abort,_Details,_ErrorUrl},#state{is_auth=false}=State) ->
  {stop,State#state{is_auth=false}};

hndl_msg(Msg,#state{is_auth=true}=State) ->
  hndl_msg_authed(Msg,State);

hndl_msg(_Msg,State) ->
  {stop,State}.


authenticate([], _RealmName, _Details, State) ->
  {reply_stop, {abort, #{}, no_such_realm}, State};
authenticate([wampcra|_], RealmName, Details,State) -> 
  AuthId = maps:get(authid, Details, anonymous),
  ClientRoles = maps:get(roles, Details, []),
  case erwa_user_db:can_join(AuthId, RealmName, tcp ) of 
    {true, Role} -> 
      case erwa_sess_man:register_session(RealmName) of
        {ok,SessionId} ->
          % a user that needs to authenticate
          % need to create a a challenge  
          SessionData = #{authid => AuthId, role => Role, session =>
                          SessionId},
          {_Result, ChallengeData} = erwa_user_db:wampcra_challenge(SessionData),
          ChallengeMsg = {challenge, wampcra, ChallengeData},
          %% WillPass = case Result of 
          %%              ok -> true;
          %%              _ -> false
          %%            end,
          {reply, ChallengeMsg, State#state{
                                            realm_name=RealmName, is_auth=false,
                                            client_roles=ClientRoles,id=SessionId}};
        {error,_} ->
          {reply_stop,{abort,#{},no_such_realm},State}
      end;
    false ->
      {reply_stop,{abort,#{},no_such_realm}, State}
  end;
authenticate([_|Tail], RealmName, Details, State) ->
  authenticate(Tail, RealmName, Details, State).





hndl_msg_authed({subscribe,RequestId,Options,Topic},#state{realm_name=RealmName,id=SessionId}=State) ->
  %% case erwa_middleware:check_perm(Msg,State) of
  %%   {true, _ } ->
      {ok,SubscriptionId} =
	  erwa_broker:subscribe(Topic,Options,SessionId,RealmName),
      {reply, {subscribed,RequestId,SubscriptionId}, State };
  %%   {false,Details} ->
  %%     OutDetails = maps:get(details,Details,#{}),
  %%     Error = maps:get(error,Details,not_authorized),
  %%     {reply, {error,subscribe,RequestId,OutDetails,Error}}
  %% end;

hndl_msg_authed({unsubscribe,RequestId,SubscriptionId},#state{id=SessionId,
															  realm_name=RealmName}=State) ->
  ok = erwa_broker:unsubscribe(SubscriptionId,SessionId,RealmName),
  {reply, {unsubscribed,RequestId},State};

hndl_msg_authed({publish,RequestId,Options,Topic,Arguments,ArgumentsKw},#state{realm_name=RealmName,id=SessionId}=State) ->
  {ok,PublicationId} =
  erwa_broker:publish(Topic,Options,Arguments,ArgumentsKw,SessionId,RealmName),
  case maps:get(acknowledge,Options,false) of
    true ->
      {reply,{published,RequestId,PublicationId},State};
    _ ->
      {ok,State}
  end;



hndl_msg_authed({register,RequestId,Options,ProcedureUri},#state{realm_name=Realm,id=SessionId}=State) ->
  {ok,RegistrationId} = erwa_dealer:register(ProcedureUri,Options,SessionId,Realm),
  {reply,{registered,RequestId,RegistrationId},State};

hndl_msg_authed({unregister,RequestId,RegistrationId},#state{realm_name=Realm,id=SessionId} = State) ->
  ok = erwa_dealer:unregister(RegistrationId,SessionId,Realm),
  {reply,{unregistered,RequestId} ,State};

hndl_msg_authed({call,RequestId,Options,ProcedureUri,Arguments,ArgumentsKw},#state{realm_name=Realm,calls=Calls,id=SessionId}=State) ->
  case erwa_dealer:call(ProcedureUri,RequestId,Options,Arguments,ArgumentsKw,SessionId,Realm) of
    {ok,Pid} ->
      {ok,State#state{calls=[{RequestId,Pid}|Calls]}};
    {error,procedure_not_found} ->
      {reply, {error,call,RequestId,#{},no_such_procedure}, State};
    {error,_} ->
      %% TODO: change the reply for eg partitioned calls
      {reply, {error,call,RequestId,#{},no_such_procedure}, State}
  end;

hndl_msg_authed({cancel,RequestId,Options},#state{calls=Calls}=State) ->
  case lists:keyfind(RequestId,1,Calls) of
    {RequestId,InvocationId} ->
      ok = erwa_invocation:cancel(InvocationId,Options),
      {reply,{error,call,RequestId,#{},canceled},State#state{calls=lists:keydelete(RequestId,1,Calls)}};

    false ->
      {reply, {error,call,RequestId,#{},no_such_procedure}, State};
    {error,_} ->
      %% TODO: change the reply for eg partitioned calls
      {reply, {error,call,RequestId,#{},no_such_procedure}, State}
  end;




hndl_msg_authed({error,invocation,InvocationId,Details,Error,Arguments,ArgumentsKw},#state{invocations=Invs,id=SessionId, realm_name=Realm}=State) ->
  case lists:keyfind(InvocationId,1,Invs) of
    {InvocationId,DealerId} ->
      ok = erwa_invocation:error(DealerId,Details,Error,Arguments,ArgumentsKw,SessionId,Realm),
      {ok,State#state{invocations = lists:keydelete(InvocationId,1,Invs)}};
    _ ->
      {ok,State}
  end;

hndl_msg_authed({yield,InvocationId,Options,Arguments,ArgumentsKw},#state{invocations=Invs,id=SessionId, realm_name=Realm}=State) ->
  case lists:keyfind(InvocationId,1,Invs) of
    {InvocationId,DealerId} ->
      ok = erwa_invocation:yield(DealerId,Options,Arguments,ArgumentsKw,SessionId,Realm),
      {ok,State#state{invocations = lists:keydelete(InvocationId,1,Invs)}};
    _ ->
      {ok,State}
  end;

hndl_msg_authed({goodbye,_Details,_Reason},#state{goodbye_sent=GBSent}=State) ->
  case GBSent of
    true ->
      {stop,State};
    false ->
      Msg = {goodbye,#{},goodbye_and_out},
      {reply_stop,Msg,State}
  end;

hndl_msg_authed(_Msg, State) ->
  {stop, State }.






hndl_info({invocation,DealerId,ProcedureId,Options,Arguments,ArgumentsKw},
          #state{invocation_id=ID, invocations=Invs}=State) ->
  NewState = State#state{invocations=[{ID,DealerId}|Invs],invocation_id=ID+1},
  {send,{invocation,ID,ProcedureId,Options,Arguments,ArgumentsKw},NewState};

hndl_info({interrupt,DealerId,Options},#state{invocations=Invs, client_roles=Roles}=State) ->
  {InvocationId,DealerId} = lists:keyfind(DealerId,2,Invs),
  Features = maps:get(features,maps:get(callee,Roles),#{}),
  case maps:get(call_canceling,Features,false) of
    true ->
      {send,{interrupt,InvocationId,Options},State};
    _ ->
      {ok,State#state{invocations=lists:keydelete(InvocationId,1,Invs)}}
  end;

hndl_info({result,CallRequestId,_,_,_}=Msg,#state{calls=Calls}=State) ->
  {send,Msg,State#state{calls=lists:keydelete(CallRequestId,1,Calls)}};

hndl_info({error,call,CallRequestId,_,_,_,_}=Msg,#state{calls=Calls}=State) ->
  {send,Msg,State#state{calls=lists:keydelete(CallRequestId,1,Calls)}};

hndl_info({event,_,_,_,_,_}=Msg,State) ->
  {send,Msg,State};

hndl_info(routing_closing,#state{goodbye_sent=GBsent}=State) ->
  Msg = {goodbye,#{},close_realm},
  case GBsent of
    false ->
      {send_stop,Msg,State#state{goodbye_sent=true}};
    _ ->
      {stop,State}
  end;

hndl_info(shutdown,State) ->
  {stop,State};

hndl_info(Info, State) ->
  error("unknown Msg ~p~n",[Info]),
  {ok,State}.



close_session(#state{id=SessionId,realm_name=RealmName}) ->
	ok = case SessionId of
			 none -> ok;
			 _ -> erwa_sess_man:unregister_session(),
                  erwa_broker:unsubscribe_all(SessionId,RealmName),
                  erwa_dealer:unregister_all(SessionId,RealmName)
		 end,
	ok.

-ifdef(TEST).




-endif.
