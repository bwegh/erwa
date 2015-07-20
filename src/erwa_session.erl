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
-endif.

-include("erwa_model.hrl").

-export([handle_message/2]).
-export([handle_info/2]).


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


-spec handle_message(term(), #session{}) ->
  {ok, #session{}} |
  {stop, #session{}} |
  {reply, Message :: term(), #session{}} |
  {reply_stop, Message :: term(), #session{}}.
handle_message(InMsg, State) ->
  case hndl_msg(InMsg, State) of
    {Result, OutMsg, State1} ->
      case Result of
        reply_stop ->
          close_session(State1);
        _ -> ok
      end,
      check_out_message(Result, OutMsg, State1);
    {stop, NewState} ->
      close_session(NewState),
      {stop, NewState};
    Other -> Other
  end.

-spec handle_info(term(), #session{}) ->
  {ok, #session{}} |
  {stop, #session{}} |
  {send, Message :: term(), #session{}} |
  {send_stop, Message :: term(), #session{}}.

handle_info(Info, State) ->
  case hndl_info(Info, State) of
    {Result, OutMsg, State1} ->
      case Result of
        send_stop ->
          close_session(State1);
        _ -> ok
      end,
      check_out_message(Result, OutMsg, State1);
    {stop, NewState} ->
      close_session(NewState),
      {stop, NewState};
    Other -> Other
  end.


%% @private
-spec check_out_message(atom(), term(), #session{}) -> {atom(), term(), #session{}} | {ok, #session{}}.
check_out_message(Result, Msg, State) ->
  %% case erwa_middleware:validate_out_message(Msg,State) of
  %%   false ->
  %%     {ok,State};
  %%   OutMsg ->
  %%     {Result,OutMsg,State}
  %% end.
  {Result, Msg, State}.

%% @private
hndl_msg({hello, RealmName, Details}, #session{trans = Transport} = State) ->
  AuthId = maps:get(authid, Details, anonymous),
  Roles = maps:get(roles, Details, []),
  case (AuthId == anonymous) or (AuthId == <<"anonymous">>) of
    true ->
      case erwa_user_db:allow_anonymous(RealmName, Transport) of
        true ->
          case erwa_realms:get_routing(RealmName) of
            {ok, RoutingPid} ->
              % the realm does exist
              State1 = create_session(RoutingPid, RealmName, Roles, State),
              #session{id = SessionId, broker = Broker, dealer = Dealer} = State1,
              BrokerFeat = erwa_broker:get_features(Broker),
              DealerFeat = erwa_dealer:get_features(Dealer),
              SessionData = #{authid => anonymous, role => anonymous, session =>
              SessionId},
              WelcomeMsg = {welcome, SessionId, #{agent => erwa:get_version(), roles => #{broker => BrokerFeat, dealer => DealerFeat}}},
              {reply, WelcomeMsg, State1#session{is_auth = true,
                session_data = SessionData}};
            {error, _} ->
              {reply_stop, {abort, #{}, no_such_realm}, State}
          end;
        false ->
          {reply_stop, {abort, #{}, no_such_realm}, State}
      end;
    false ->
      AuthMethods = maps:get(authmethods, Details, []),
      authenticate(AuthMethods, RealmName, Details, State)
  end;
hndl_msg({authenticate, _Signature, _Extra} = Msg, State = #session{id = SessionId, broker = Broker, dealer = Dealer}) ->
  BrokerFeat = erwa_broker:get_features(Broker),
  DealerFeat = erwa_dealer:get_features(Dealer),
  Msg = {welcome, SessionId, #{agent => erwa:get_version(), roles => #{broker => BrokerFeat, dealer => DealerFeat}}},
  {reply, Msg, State#session{is_auth = true}};
hndl_msg({abort, _Details, _ErrorUrl}, #session{is_auth = false} = State) ->
  {stop, State#session{is_auth = false}};
hndl_msg(Msg, #session{is_auth = true} = State) ->
  hndl_msg_authed(Msg, State);
hndl_msg(_Msg, State) ->
  {stop, State}.

%% @private
authenticate([], _RealmName, _Details, State) ->
  {reply_stop, {abort, #{}, no_such_realm}, State};
authenticate([wampcra | _], RealmName, Details, #session{trans = Transport} = State) ->
  AuthId = maps:get(authid, Details, anonymous),
  Roles = maps:get(roles, Details, []),
  case erwa_user_db:can_join(AuthId, RealmName, Transport) of
    {true, Role} ->
      case erwa_realms:get_routing(RealmName) of
        {ok, RoutingPid} ->
          % the realm does exist
          State1 = create_session(RoutingPid, RealmName, Roles, State),
          #session{id = SessionId} = State1,
          % a user that needs to authenticate
          % need to create a a challenge
          SessionData = #{authid => AuthId, role => Role, session =>
          SessionId},
          {Result, ChallengeData} = erwa_user_db:wampcra_challenge(SessionData),
          ChallengeMsg = {challenge, wampcra, ChallengeData},
          WillPass = case Result of
                       ok -> true;
                       _ -> false
                     end,
          {reply, ChallengeMsg, State1#session{authid = AuthId, role = Role,
            will_pass = WillPass,
            session_data = SessionData}};
        {error, _} ->
          {reply_stop, {abort, #{}, no_such_realm}, State}
      end;
    false ->
      {reply_stop, {abort, #{}, no_such_realm}, State}
  end;
authenticate([_ | Tail], RealmName, Details, State) ->
  authenticate(Tail, RealmName, Details, State).

%% @private
create_session(RoutingPid, RealmName, Roles, State) ->
  {ok, SessionId} = erwa_sessions:register_session(RealmName),
  ok = erwa_routing:connect(RoutingPid, State),
  {ok, Broker} = erwa_routing:get_broker(RoutingPid),
  {ok, Dealer} = erwa_routing:get_dealer(RoutingPid),
  {ok, MWL} = erwa_realms:get_middleware_list(RealmName),
  State#session{id = SessionId, mwl = MWL, realm_name = RealmName, routing_pid = RoutingPid,
    is_auth = false, dealer = Dealer, broker = Broker, client_roles = Roles}.

%% @private
hndl_msg_authed({subscribe, RequestId, Options, Topic}, #session{broker = Broker, id = SessionId} = State) ->
  {ok, SubscriptionId} = erwa_broker:subscribe(Topic, Options, SessionId, Broker),
  {reply, {subscribed, RequestId, SubscriptionId}, State};
hndl_msg_authed({unsubscribe, RequestId, SubscriptionId}, #session{broker = Broker, id = SessionId} = State) ->
  ok = erwa_broker:unsubscribe(SubscriptionId, SessionId, Broker),
  {reply, {unsubscribed, RequestId}, State};
hndl_msg_authed({publish, RequestId, Options, Topic, Arguments, ArgumentsKw}, #session{broker = Broker, id = SessionId} = State) ->
  {ok, PublicationId} = erwa_broker:publish(Topic, Options, Arguments, ArgumentsKw, SessionId, Broker),
  case maps:get(acknowledge, Options, false) of
    true ->
      {reply, {published, RequestId, PublicationId}, State};
    _ ->
      {ok, State}
  end;
hndl_msg_authed({register, RequestId, Options, ProcedureUri}, #session{dealer = Dealer, id = SessionId} = State) ->
  {ok, RegistrationId} = erwa_dealer:register(ProcedureUri, Options, SessionId, Dealer),
  {reply, {registered, RequestId, RegistrationId}, State};
hndl_msg_authed({unregister, RequestId, RegistrationId}, #session{dealer = Dealer, id = SessionId} = State) ->
  ok = erwa_dealer:unregister(RegistrationId, SessionId, Dealer),
  {reply, {unregistered, RequestId}, State};
hndl_msg_authed({call, RequestId, Options, ProcedureUri, Arguments, ArgumentsKw}, #session{dealer = Dealer, calls = Calls, id = SessionId} = State) ->
  case erwa_dealer:call(ProcedureUri, RequestId, Options, Arguments, ArgumentsKw, SessionId, Dealer) of
    {ok, Pid} ->
      {ok, State#session{calls = [{RequestId, Pid} | Calls]}};
    {error, procedure_not_found} ->
      {reply, {error, call, RequestId, #{}, no_such_procedure}, State};
    {error, _} ->
      %% TODO: change the reply for eg partitioned calls
      {reply, {error, call, RequestId, #{}, no_such_procedure}, State}
  end;
hndl_msg_authed({cancel, RequestId, Options}, #session{calls = Calls} = State) ->
  case lists:keyfind(RequestId, 1, Calls) of
    {RequestId, Pid} ->
      ok = erwa_invocation:cancel(Pid, Options),
      {reply, {error, call, RequestId, #{}, canceled}, State#session{calls = lists:keydelete(RequestId, 1, Calls)}};
    false ->
      {reply, {error, call, RequestId, #{}, no_such_procedure}, State};
    {error, _} ->
      %% TODO: change the reply for eg partitioned calls
      {reply, {error, call, RequestId, #{}, no_such_procedure}, State}
  end;
hndl_msg_authed({error, invocation, InvocationId, Details, Error, Arguments, ArgumentsKw}, #session{invocations = Invs, id = SessionId} = State) ->
  case lists:keyfind(InvocationId, 1, Invs) of
    {InvocationId, Pid} ->
      ok = erwa_invocation:error(Pid, Details, Error, Arguments, ArgumentsKw, SessionId),
      {ok, State#session{invocations = lists:keydelete(InvocationId, 1, Invs)}};
    _ ->
      {ok, State}
  end;
hndl_msg_authed({yield, InvocationId, Options, Arguments, ArgumentsKw}, #session{invocations = Invs, id = SessionId} = State) ->
  case lists:keyfind(InvocationId, 1, Invs) of
    {InvocationId, Pid} ->
      ok = erwa_invocation:yield(Pid, Options, Arguments, ArgumentsKw, SessionId),
      {ok, State#session{invocations = lists:keydelete(InvocationId, 1, Invs)}};
    _ ->
      {ok, State}
  end;
hndl_msg_authed({goodbye, _Details, _Reason}, #session{goodbye_sent = GBSent} = State) ->
  case GBSent of
    true ->
      {stop, State};
    false ->
      Msg = {goodbye, #{}, goodbye_and_out},
      {reply_stop, Msg, State}
  end;
hndl_msg_authed(_Msg, State) ->
  {stop, State}.

%% @private
hndl_info({invocation, set_request_id, ProcedureId, Options, Arguments, ArgumentsKw},
    State = #session{invocation_id = ID, invocations = Invs}) ->
  Pid = maps:get(invocation_pid, Options),
  Options1 = maps:remove(invocation_pid, Options),
  NewState = State#session{invocations = [{ID, Pid} | Invs], invocation_id = ID + 1},
  {send, {invocation, ID, ProcedureId, Options1, Arguments, ArgumentsKw}, NewState};
hndl_info({interrupt, set_request_id, Options}, State = #session{invocations = Invs, client_roles = Roles, id = SessionId}) ->
  Pid = maps:get(invocation_pid, Options),
  {InvocationId, Pid} = lists:keyfind(Pid, 2, Invs),
  Features = maps:get(features, maps:get(callee, Roles), #{}),
  case maps:get(call_canceling, Features, false) of
    true ->
      {send, {interrupt, InvocationId, lists:keydelete(invocation_pid, 1, Options)}, State};
    _ ->
      ok = erwa_invocation:error(Pid, #{}, canceled, undefined, undefined, SessionId),
      {ok, State#session{invocations = lists:keydelete(InvocationId, 1, Invs)}}
  end;
hndl_info(Msg = {result, CallRequestId, _, _, _}, State = #session{calls = Calls}) ->
  {send, Msg, State#session{calls = lists:keydelete(CallRequestId, 1, Calls)}};
hndl_info(Msg = {error, call, CallRequestId, _, _, _, _}, State = #session{calls = Calls}) ->
  {send, Msg, State#session{calls = lists:keydelete(CallRequestId, 1, Calls)}};
hndl_info({event, _, _, _, _, _} = Msg, State) ->
  {send, Msg, State};
hndl_info(routing_closing, State = #session{goodbye_sent = false}) ->
  Msg = {goodbye, #{}, close_realm},
  {send_stop, Msg, State#session{goodbye_sent = true}};
hndl_info(routing_closing, State) ->
  {stop, State};
hndl_info(shutdown, State) ->
  {stop, State};
hndl_info(Info, State) ->
  error("unknown Msg ~p~n", [Info]),
  {ok, State}.

%% @private
close_session(#session{broker = Broker, dealer = Dealer, routing_pid = RoutingPid, id = SessionId}) ->
  ok = unregister(Broker, fun() -> erwa_broker:unsubscribe_all(SessionId, Broker) end),
  ok = unregister(Dealer, fun() -> erwa_dealer:unregister_all(SessionId, Broker) end),
  ok = unregister(RoutingPid, fun() -> erwa_routing:disconnect(RoutingPid) end),
  ok = unregister(SessionId, fun() -> erwa_sessions:unregister_session() end).

%% @private
unregister(none, _) -> ok;
unregister(_, Terminator) ->
  Terminator().