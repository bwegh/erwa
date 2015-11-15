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

-include("elogger.hrl").
-export([init/0]).
-export([close/0]).
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
                client_role = anonymous 
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
    {ok, Id} = erwa_sess_man:create_session(),
  #state{id=Id}.

close() ->
    erwa_sess_man:unregister_session(),
    ok.

-spec handle_message( term() , #state{}) ->
  { ok, #state{} } |
  {stop, #state{}} |
  {reply, Message:: term(), #state{}} |
  {reply_stop, Message:: term(), #state{} }.


handle_message(InMsg,State) ->
  ?DEBUG("incomming message ~p [~p]~n",[InMsg,State]),
  send_message_or_close_session(hndl_msg(InMsg,State)).


-spec handle_info( term() , #state{}) ->
  { ok, #state{} } |
  {stop, #state{}} |
  {send, Message:: term(), #state{}} |
  {send_stop, Message:: term(), #state{} }.

handle_info(Info,State) ->
  ?DEBUG("incomming info ~p [~p]~n",[Info,State]),
  send_message_or_close_session(hndl_info(Info,State)).


send_message_or_close_session({reply_stop,OutMsg,State}) ->
  close_session(State),
  check_and_return_out_message(reply_stop,OutMsg,State);
send_message_or_close_session({send_stop,OutMsg,State}) ->
  close_session(State),
  check_and_return_out_message(send_stop,OutMsg,State);
send_message_or_close_session({Result,OutMsg,State}) ->
  check_and_return_out_message(Result, OutMsg, State);
send_message_or_close_session({ok, State}) ->
  {ok, State};
send_message_or_close_session({stop,State}) ->
  close_session(State),
  {stop, State}.


-spec check_and_return_out_message(atom(), term(), #state{} ) -> {atom(), term(),
                                                            #state{} } |
                                                           {ok, #state{} }.
check_and_return_out_message(Result, Msg,State) ->
  % TODO: implement
  {Result, Msg, State}.



-spec hndl_msg(term(),#state{}) -> {stop, #state{}} | {reply_stop | reply , term(),
                                              #state{}}.

hndl_msg({hello,RealmName,Details}, State) ->
  handle_hello_message(RealmName, Details, State);
hndl_msg({authenticate,Signature,Extra},State) ->
  handle_authenticate_message(Signature,Extra,State);

hndl_msg({abort,_Details,_ErrorUrl},#state{is_auth=false}=State) ->
  {stop,State#state{is_auth=false}};

hndl_msg(Msg,#state{is_auth=true}=State) ->
  hndl_msg_authed(Msg,State);

hndl_msg(_Msg,State) ->
  {stop,State}.



hndl_msg_authed({subscribe,RequestId,Options,Topic},State) ->
  handle_subscribe_message(RequestId,Options,Topic,State);
hndl_msg_authed({unsubscribe,RequestId,SubscriptionId},State) ->
  handle_unsubscribe_message(RequestId, SubscriptionId, State);
hndl_msg_authed({publish,RequestId,Options,Topic,Arguments,ArgumentsKw},State) ->
  handle_publish_message(RequestId,Options,Topic,Arguments, ArgumentsKw, State);
hndl_msg_authed({register,RequestId,Options,ProcedureUri},State) ->
  handle_register_message(RequestId,Options,ProcedureUri,State);
hndl_msg_authed({unregister,RequestId,RegistrationId}, State) ->
  handle_unregister_message(RequestId,RegistrationId,State);
hndl_msg_authed({call,RequestId,Options,ProcedureUri,Arguments,ArgumentsKw},State) ->
  handle_call_message(RequestId,Options, ProcedureUri, Arguments, ArgumentsKw, State);
hndl_msg_authed({cancel,RequestId,Options},State) ->
  handle_cancel_message(RequestId,Options,State);
hndl_msg_authed({error,invocation,InvocationId,Details,Error,Arguments,ArgumentsKw},State) ->
  handle_error_invocation_message(InvocationId,Details,Error,Arguments,ArgumentsKw,State);
hndl_msg_authed({yield,InvocationId,Options,Arguments,ArgumentsKw},State) ->
  handle_yield_message(InvocationId,Options,Arguments,ArgumentsKw,State);
hndl_msg_authed({goodbye,Details,Reason},State) ->
  handle_goodbye_message(Details,Reason,State);
hndl_msg_authed(Msg, State) ->
  handle_unknown_message(Msg,State).


hndl_info({invocation,DealerId,ProcedureId,Options,Arguments,ArgumentsKw}, State) ->
  handle_invocation_info(DealerId,ProcedureId,Options,Arguments,ArgumentsKw, State);
hndl_info({interrupt,DealerId,Options},State) ->
  handle_interrupt_info(DealerId,Options,State);
hndl_info({result,CallRequestId,Details,Arguments,ArgumentsKw},State) ->
  handle_result_info(CallRequestId,Details,Arguments,ArgumentsKw,State);
hndl_info({error,call,CallRequestId,Details,ErrorUri,Arguments,ArgumentsKw},State) ->
  hndl_error_call_info(error,call,CallRequestId,Details,ErrorUri,Arguments,ArgumentsKw,State);
hndl_info({event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw}, State) ->
  handl_event_info(SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw,State);
hndl_info(routing_closing,State) ->
  handle_routing_closing_info(State); 
hndl_info(shutdown,State) ->
  handle_shutdown_info(State);
hndl_info(Info, State) ->
  handle_unknown_info(Info, State).




%%%%% hello message %%%%%%%%%%%%%%%%%%%%%
handle_hello_message(RealmName, Details, State) ->
  Args = gather_hello_args(Details,RealmName),
  reply_to_hello(check_authentication(Args),Args,State).

reply_to_hello(anonymous, #{ realm := RealmName }, #state{id=SessionId}=State) ->
      case erwa_sess_man:connect_to(RealmName) of
        ok ->
          %% SessionData = #{authid => anonymous, role => anonymous, session =>
          %%                 SessionId},
          WelcomeMsg ={welcome,SessionId,#{agent => erwa:get_version(), roles =>
										  ?ROLES}},
          {reply,WelcomeMsg,State#state{is_auth=true,
                                        realm_name=RealmName,
                                        client_role=anonymous,
                                        id=SessionId
                                        }}; 
        {error,_} ->
              erwa_sess_man:unregister_session(),
          {reply_stop, {abort, #{}, no_such_realm},State}
      end;

reply_to_hello(authenticate,#{details := _Details},State) ->
  ?ERROR("authentication not yet implemented~n",[]),
  %% AuthMethods = maps:get(authmethods, Details, []),
  %% authenticate(AuthMethods, RealmName, Details, State)
  erwa_sess_man:unregister_session(),
  {reply_stop, {abort, #{}, no_such_realm}, State};

reply_to_hello(abort,_,State) ->
  erwa_sess_man:unregister_session(),
  {reply_stop, {abort, #{}, no_such_realm}, State}.


gather_hello_args(Details, RealmName) ->
  AuthId = maps:get(authid, Details, anonymous),
  #{auth_id => AuthId, realm => RealmName, details => Details}.

check_authentication(#{realm := RealmName} = Args) -> 
  get_authentication_mechanism(Args, erwa_user_db:allow_anonymous(RealmName, tcp)).

get_authentication_mechanism(#{auth_id := anonymous}, true) -> 
  anonymous;
get_authentication_mechanism(#{auth_id := anonymous}, _) -> 
  abort;
get_authentication_mechanism(_, _) -> 
  authenticate.
  

%%%%%%%% authenticate message %%%%%%%%%%%%%%%%
handle_authenticate_message(_Signature,_Extra,State) ->
  % case erwa_middleware:check_perm(Msg,State) of
  %  {true,_} ->
      #state{id=SessionId} = State,
      Msg ={welcome,SessionId,#{agent => erwa:get_version(), roles => ?ROLES }},
      {reply,Msg,State#state{is_auth=true}}.
  %%   {false,Details} ->
  %%     OutDetails = maps:get(details,Details,#{}),
  %%     Error = maps:get(error,Details,#{}),
  %%     Msg = {abort,OutDetails,Error},
  %%     {reply_stop,Msg,State}
  %% end;


%% authenticate([], _RealmName, _Details, State) ->
%%   {reply_stop, {abort, #{}, no_such_realm}, State};
%% authenticate([wampcra|_], RealmName, Details,State) -> 
%%   AuthId = maps:get(authid, Details, anonymous),
%%   ClientRoles = maps:get(roles, Details, []),
%%   case erwa_user_db:can_join(AuthId, RealmName, tcp ) of 
%%     {true, Role} -> 
%%       case erwa_sess_man:register_session(RealmName) of
%%         {ok,SessionId} ->
%%           % a user that needs to authenticate
%%           % need to create a a challenge  
%%           SessionData = #{authid => AuthId, role => Role, session =>
%%                           SessionId},
%%           {_Result, ChallengeData} = erwa_user_db:wampcra_challenge(SessionData),
%%           ChallengeMsg = {challenge, wampcra, ChallengeData},
%%           %% WillPass = case Result of 
%%           %%              ok -> true;
%%           %%              _ -> false
%%           %%            end,
%%           {reply, ChallengeMsg, State#state{
%%                                             realm_name=RealmName, is_auth=false,
%%                                             client_roles=ClientRoles,id=SessionId}};
%%         {error,_} ->
%%           {reply_stop,{abort,#{},no_such_realm},State}
%%       end;
%%     false ->
%%       {reply_stop,{abort,#{},no_such_realm}, State}
%%   end;
%% authenticate([_|Tail], RealmName, Details, State) ->
%%   authenticate(Tail, RealmName, Details, State).




%%%%%%%%%%%%% subscribe message %%%%%%%%%%%%%%%%%%%%
handle_subscribe_message(RequestId,Options,Topic,#state{realm_name=RealmName,id=SessionId}=State) ->
  %% case erwa_middleware:check_perm(Msg,State) of
  %%   {true, _ } ->
      {ok,SubscriptionId} =
	  erwa_broker:subscribe(Topic,Options,SessionId,RealmName),
      {reply, {subscribed,RequestId,SubscriptionId}, State }.
  %%   {false,Details} ->
  %%     OutDetails = maps:get(details,Details,#{}),
  %%     Error = maps:get(error,Details,not_authorized),
  %%     {reply, {error,subscribe,RequestId,OutDetails,Error}}
  %% end;



%%%%%%%%%%%% unsubscribe message  %%%%%%%%%%%%%%%%%%%%%%
handle_unsubscribe_message(RequestId,SubscriptionId,#state{id=SessionId, realm_name=RealmName}=State) ->
  ok = erwa_broker:unsubscribe(SubscriptionId,SessionId,RealmName),
  {reply, {unsubscribed,RequestId},State}.


%%%%%%%%%%%%5 publish message %%%%%%%%%%%%%%
handle_publish_message(RequestId,Options,Topic,Arguments,ArgumentsKw,#state{realm_name=RealmName,id=SessionId}=State) ->
  {ok,PublicationId} = erwa_broker:publish(Topic,Options,Arguments,ArgumentsKw,SessionId,RealmName),
  case maps:get(acknowledge,Options,false) of
    true ->
      {reply,{published,RequestId,PublicationId},State};
    _ ->
      {ok,State}
  end.


%%%%%%%%%%%%%%%%%%% register message %%%%%%%%%%%%%%%%%%%
handle_register_message(RequestId,Options,ProcedureUri,#state{realm_name=Realm,id=SessionId}=State) ->
  {ok,RegistrationId} = erwa_dealer:register(ProcedureUri,Options,SessionId,Realm),
  {reply,{registered,RequestId,RegistrationId},State}.




%%%%%%%%%%%%%%% unregister message %%%%%%%%%%%%%%%%%%%%%%%
handle_unregister_message(RequestId,RegistrationId,#state{realm_name=Realm,id=SessionId} = State) ->
  ok = erwa_dealer:unregister(RegistrationId,SessionId,Realm),
  {reply,{unregistered,RequestId} ,State}.

%%%%%%%%%%%%%%% call message %%%%%%%%%%%%%%%%%%%%%%%%%
handle_call_message(RequestId,Options,ProcedureUri,Arguments,ArgumentsKw,#state{realm_name=Realm,calls=Calls,id=SessionId}=State) ->
  case erwa_dealer:call(ProcedureUri,RequestId,Options,Arguments,ArgumentsKw,SessionId,Realm) of
    {ok,Pid} ->
      {ok,State#state{calls=[{RequestId,Pid}|Calls]}};
    {error,procedure_not_found} ->
      {reply, {error,call,RequestId,#{},no_such_procedure}, State};
    {error,_} ->
      %% TODO: change the reply for eg partitioned calls
      {reply, {error,call,RequestId,#{},no_such_procedure}, State}
  end.

%%%%%%%%%%%%%%% cancel message %%%%%%%%%%%%%%%%%%%%%%%%%
handle_cancel_message(RequestId,Options,#state{calls=Calls}=State) ->
  case lists:keyfind(RequestId,1,Calls) of
    {RequestId,InvocationId} ->
      ok = erwa_invocation:cancel(InvocationId,Options),
      {reply,{error,call,RequestId,#{},canceled},State#state{calls=lists:keydelete(RequestId,1,Calls)}};
    false ->
      {reply, {error,call,RequestId,#{},no_such_procedure}, State};
    {error,_} ->
      %% TODO: change the reply for eg partitioned calls
      {reply, {error,call,RequestId,#{},no_such_procedure}, State}
  end.


%%%%%%%%%%%%% error invocation message %%%%%%%%%%%%%%%%%%%%%
handle_error_invocation_message(InvocationId,Details,Error,Arguments,ArgumentsKw,#state{invocations=Invs,id=SessionId, realm_name=Realm}=State) ->
  case lists:keyfind(InvocationId,1,Invs) of
    {InvocationId,DealerId} ->
      ok = erwa_invocation:error(DealerId,Details,Error,Arguments,ArgumentsKw,SessionId,Realm),
      {ok,State#state{invocations = lists:keydelete(InvocationId,1,Invs)}};
    _ ->
      {ok,State}
  end.



%%%%%%%%%%%%% yield messag messaee %%%%%%%%%%%%%%%%%%%%%%%%%%5
handle_yield_message(InvocationId,Options,Arguments,ArgumentsKw,#state{invocations=Invs,id=SessionId, realm_name=Realm}=State) ->
  case lists:keyfind(InvocationId,1,Invs) of
    {InvocationId,DealerId} ->
      ok = erwa_invocation:yield(DealerId,Options,Arguments,ArgumentsKw,SessionId,Realm),
      {ok,State#state{invocations = lists:keydelete(InvocationId,1,Invs)}};
    _ ->
      {ok,State}
  end.


%%%%%%%%%%%%% goodbye message %%%%%%%%%%%%%%%%%
handle_goodbye_message(_Details,_Reason,#state{goodbye_sent=GBSent}=State) ->
  case GBSent of
    true ->
      {stop,State};
    false ->
      Msg = {goodbye,#{},goodbye_and_out},
      {reply_stop,Msg,State}
  end.


%%%%%%%%%%% unkonwn messages %%%%%%%%%%%%%%%%%%
handle_unknown_message(Msg,State) ->
  ?WARNING("unknown message ~p~n",[Msg]),
  {stop, State }.



%%%%%%%% invocation info %%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_invocation_info(DealerId,ProcedureId,Options,Arguments,ArgumentsKw, #state{invocation_id=ID, invocations=Invs}=State) ->
  NewState = State#state{invocations=[{ID,DealerId}|Invs],invocation_id=ID+1},
  {send,{invocation,ID,ProcedureId,Options,Arguments,ArgumentsKw},NewState}.



%%%%%%%%%%% interrupt info %%%%%%%%%%%%%%%%%%%
handle_interrupt_info(DealerId,Options,#state{invocations=Invs, client_role=Role}=State) ->
  {InvocationId,DealerId} = lists:keyfind(DealerId,2,Invs),
  Features = maps:get(features,maps:get(callee,Role),#{}),
  case maps:get(call_canceling,Features,false) of
    true ->
      {send,{interrupt,InvocationId,Options},State};
    _ ->
      {ok,State#state{invocations=lists:keydelete(InvocationId,1,Invs)}}
  end.


%%%%%%%%%%%% result info %%%%%%%%%%%%%%%%%%%%
handle_result_info(CallRequestId,Details,Arguments,ArgumentsKw,#state{calls=Calls}=State) ->
  Msg = {result,CallRequestId,Details,Arguments,ArgumentsKw},
  {send,Msg,State#state{calls=lists:keydelete(CallRequestId,1,Calls)}}.


%%%%%%%%%%%% error call info %%%%%%%%%%%%%%%%%
hndl_error_call_info(error,call,CallRequestId,Details,ErrorUri,Arguments,ArgumentsKw,#state{calls=Calls}=State) ->
  Msg = {error, call, CallRequestId,Details,ErrorUri,Arguments,ArgumentsKw}, 
  {send,Msg,State#state{calls=lists:keydelete(CallRequestId,1,Calls)}}.


%%%%%%%%%%%% event info %%%%%%%%%%%%%%%%%%
handl_event_info(SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw,State) ->
  Msg = {event, SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw},
  {send,Msg,State}.


%%%%%%%%%%%%%%%%% routing closing info %%%%%%%%%%%%%%%%%%%
handle_routing_closing_info(#state{goodbye_sent=GBsent}=State) ->
  Msg = {goodbye,#{},close_realm},
  case GBsent of
    false ->
      {send_stop,Msg,State#state{goodbye_sent=true}};
    _ ->
      {stop,State}
  end.


%%%%%%%%%% shutdown info %%%%%%%%%%%%%%%%
handle_shutdown_info(State) ->
  {stop,State}.

%%%%%%%%%%%% unknown info %%%%%%%%%%%%%%%%
handle_unknown_info(Info, State) ->
  ?WARNING("unknown info ~p~n",[Info]),
  {ok,State}.


close_session(#state{id=SessionId,realm_name=RealmName}) ->
    erwa_sess_man:unregister_session(),
    erwa_broker:unsubscribe_all(SessionId,RealmName),
    erwa_dealer:unregister_all(SessionId,RealmName).

