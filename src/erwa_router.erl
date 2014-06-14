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
-module(erwa_router).
-behaviour(gen_server).



-export([shutdown/1]).

-export([remove_session/2]).

-export([hello/2]).
-export([goodbye/3]).
%-export([error/3]).

-export([publish/4,publish/5,publish/6]).
-export([subscribe/4]).
-export([unsubscribe/3]).


-export([register/4]).
-export([unregister/3]).
-export([call/6]).
-export([yield/5]).


%% API.
-export([start/1]).
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



shutdown(Router) ->
  gen_server:cast(Router,shutdown).

hello(Router,Details) ->
  gen_server:call(Router,{hello,Details}).

goodbye(Router,Details,Reason) ->
  gen_server:call(Router,{goodbye,Details,Reason}).

remove_session(Router,Reason) ->
  gen_server:call(Router,{remove_session,Reason}).


subscribe(Router,RequestId,Options,Topic) when is_pid(Router) ->
  gen_server:call(Router,{subscribe,RequestId,Options,Topic}).

publish(Router,RequestId,Options,Topic) ->
  publish(Router,RequestId,Options,Topic,undefined,undefined).
publish(Router,RequestId,Options,Topic,Arguments)->
  publish(Router,RequestId,Options,Topic,Arguments,undefined).
publish(Router,RequestId,Options,Topic,Arguments,ArgumentsKw) when is_pid(Router) ->
  gen_server:call(Router,{publish,RequestId,Options,Topic,Arguments,ArgumentsKw}).


unsubscribe(Router,RequestId,SubscriptionId) when is_pid(Router) ->
  gen_server:call(Router,{unsubscribe,RequestId,SubscriptionId}).



register(Router,RequestId,Options,Procedure) when is_pid(Router) ->
  gen_server:call(Router,{register,RequestId,Options,Procedure}).

unregister(Router,RequestId,RegistrationId) when is_pid(Router) ->
  gen_server:call(Router,{unregister,RequestId,RegistrationId}).

call(Router,RequestId,Options,ProcedureUrl,Arguments,ArgumentsKw) when is_pid(Router) ->
  gen_server:call(Router,{call,RequestId,Options,ProcedureUrl,Arguments,ArgumentsKw}).

yield(Router,InvocationId,Options,Arguments,ArgumentsKw) when is_pid(Router) ->
  gen_server:call(Router,{yield,InvocationId,Options,Arguments,ArgumentsKw}).



start(Args) ->
  gen_server:start(?MODULE, [Args], []).

start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).


-define(ROUTER_DETAILS,[{<<"roles">>,[{<<"broker">>,[{}]},{<<"dealer">>,[{}]}]}]).


-record(state, {
	realm = undefined,
  sess = undefined,
  pubsub = undefined,
  rpc = undefined,
  t = undefined,
  p = undefined,
  i = undefined,
  ut = undefined,
  up = undefined
}).

-record(session, {
  id = undefined,
  pid = undefined,
  details = undefined,
  requestId = 1,
  goodbye_sent = false,
  subscriptions = [],
  registrations = []
}).

-record(pid_session, {
  pid = undefined,
  session_id = undefined
}).

-record(ref_session, {
  ref = undefined,
  session_id = undefined
}).


-record(topic, {
  id = undefined,
  url = undefined,
  publishId = 1,
  subscribers = [],
  options = undefined
}).

%-record(subscription, {
%  id = undefined,
%  topicId = undefined,
%  options = undefined}).

-record(procedure, {
  id = undefined,
  url = undefined,
  options = undefined,
  session_id = undefined
}).

-record(invocation, {
  timestamp = undefined,
  id = undefined,
  callee_id = undefined,
  procedure_id = undefined,
  request_id = undefined,
  caller_id = undefined,
  options = undefined,
  arguments = undefined,
  argumentskw = undefined
}).

%% gen_server.

-define(TABLE_ACCESS,protected).

-spec init(Params :: list() ) -> {ok,#state{}}.
init([Realm]) ->

  T = ets:new(erwa_topics,[?TABLE_ACCESS,set,{keypos,#topic.id}]),
  UT = ets:new(erwa_url_topic,[?TABLE_ACCESS,set]),
  %Su = ets:new(subscriptions,[?TABLE_ACCESS,set,{keypos,#subscription.id}]),


  P = ets:new(erwa_procedures,[?TABLE_ACCESS,set,{keypos,#procedure.id}]),
  UP = ets:new(erwa_url_procedures,[?TABLE_ACCESS,set]),
  %R = ets:new(erwa_registrations,[?TABLE_ACCESS,set]),
  I = ets:new(erwa_invocations,[?TABLE_ACCESS,set,{keypos,#invocation.id}]),

  Sess = ets:new(erwa_session_management,[?TABLE_ACCESS,set,{keypos,2}]),
  PubSub = ets:new(erwa_publication_subscription,[?TABLE_ACCESS,set,{keypos,2}]),
  RPC = ets:new(erwa_remote_procedure_calls,[?TABLE_ACCESS,set,{keypos,2}]),

  {ok,#state{realm=Realm,t=T,p=P,i=I,ut=UT,up=UP,sess=Sess,pubsub=PubSub,rpc=RPC}}.



-spec handle_call(Msg :: term(), From :: term(), #state{}) -> {reply,Msg :: term(), #state{}}.
handle_call({hello,Details}, {Pid,_Ref}, #state{sess=Sess}=State) ->
  Reply =
    case ets:member(Sess,Pid) of
      true ->
        %hello from an already connected client -> shutdown (as specified)
        shutdown;
      false ->
        {ok,Id} = create_session(Pid,Details,State),
        {welcome,Id,?ROUTER_DETAILS}
    end,
  {reply,Reply,State};

handle_call({goodbye,_Details,_Reason},{Pid,_Ref},#state{sess=Sess}=State) ->
  Session = get_session_from_pid(Pid,State),
  SessionId = Session#session.id,
  Reply =
    case Session#session.goodbye_sent of
      true -> shutdown;
      _ ->
        ets:update_element(Sess,SessionId,{#session.goodbye_sent,true}),
        %TODO: send a message after a timeout to close the session
        %send_message_to_peers({erwar,shutdown},[SessionId]),
        {goodbye,[{}],goodbye_and_out}
    end,
  {reply,Reply,State};

handle_call({subscribe,RequestId,Options,TopicUrl}, {Pid,_Ref}, State) ->
  {ok,SubscriptionId} = subscribe_to_topic(Pid,Options,TopicUrl,State),
  {reply,{subscribed,RequestId,SubscriptionId},State};

handle_call({unsubscribe,RequestId,SubscriptionId}, {Pid,_Ref} , State) ->
  Reply=
  case unsubscribe_from_topic(Pid,SubscriptionId,State) of
    {ok} -> {unsubscribed,RequestId};
    {error,Details,Reason} -> {error,unsubscribe,RequestId,Details,Reason}
  end,
  {reply,Reply,State};

handle_call({publish,RequestId,Options,Url,Arguments,ArgumentsKw}, {Pid,_Ref}, State) ->
  {ok,PublicationId}= send_event_to_topic(Pid,Options,Url,Arguments,ArgumentsKw,State),
  Reply =
  case lists:member({acknowledge,true},Options) of
    true -> {published,RequestId,PublicationId};
    _ -> noreply
  end,
  {reply,Reply,State};

handle_call({register,RequestId,Options,Procedure},{Pid,_Ref},State) ->
  Reply =
    case register_procedure(Pid,Options,Procedure,State) of
      {ok,RegistrationId} -> {registered,RequestId,RegistrationId};
      {error,Details,Reason} -> {error,register,RequestId,Details,Reason}
    end,
  {reply,Reply,State};

handle_call({unregister,RequestId,RegistrationId},{Pid,_Ref},State) ->
  Reply =
    case unregister_procedure(Pid,RegistrationId,State) of
      {ok} -> {unregistered,RequestId};
      {error,Details,Reason} -> {error,unregister,RequestId,Details,Reason}
    end,
  {reply,Reply,State};

handle_call({call,RequestId,Options,ProcedureUrl,Arguments,ArgumentsKw},{Pid,_Ref},State) ->
  Reply =
  case enqueue_procedure_call(Pid,RequestId,Options,ProcedureUrl,Arguments,ArgumentsKw,State) of
    {ok} -> noreply;
    {error,Details,Reason} -> {error,call,RequestId,Details,Reason,Arguments,ArgumentsKw}
  end,
  {reply,Reply,State};

handle_call({yield,RequestId,Options,Arguments,ArgumentsKw},{Pid,_Ref},State) ->
  Reply =
    case dequeue_procedure_call(Pid,RequestId,Options,Arguments,ArgumentsKw,State) of
      {error,not_found} ->
        noreply;
      {ok} ->
        noreply;
      _ ->
        shutdown
      end,
  {reply,Reply,State};

handle_call(_Msg,_From,State) ->
   {reply,shutdown,State}.


handle_cast(shutdown, State) ->
  %TODO: implement
  {stop, normal, State};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'DOWN',Ref,process,_Pid,_Reason},State) ->
  remove_session_with_ref(Ref,State),
  {noreply,State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


-spec create_session(Pid :: pid(), Details :: list(), State :: #state{}) -> {ok,non_neg_integer()}.
create_session(Pid,Details,#state{sess=Sessions}=State) ->
  Id = gen_id(),
  Ref = monitor(process,Pid),
  case ets:insert_new(Sessions,[#session{id=Id,pid=Pid,details=Details},
                                #ref_session{ref=Ref,session_id=Id},
                                #pid_session{pid=Pid,session_id=Id}]) of
    true ->
      {ok,Id};
    _ ->
      demonitor(Ref),
      create_session(Pid,Details,State)
  end.


-spec subscribe_to_topic(Pid :: pid(), Options :: list(), Url :: binary(), State :: #state{}) -> {ok, non_neg_integer()}.
subscribe_to_topic(Pid,Options,Url,#state{sess=Sessions,t=Topics,ut=UT}=State) ->
  Session = get_session_from_pid(Pid,State),
  SessionId = Session#session.id,
  Subs = Session#session.subscriptions,
  Topic =
    case ets:lookup(UT,Url) of
      [] ->
        % create the topic ...
        {ok,T} = create_topic(Url,Options,State),
        T;
      [{Url,Id}] ->
        [T] = ets:lookup(Topics,Id),
        T
    end,
  #topic{id=TopicId,subscribers=Subscribers} = Topic,
  ets:update_element(Topics,TopicId,{#topic.subscribers,[SessionId|lists:delete(SessionId,Subscribers)]}),
  ets:update_element(Sessions,SessionId,{#session.subscriptions,[TopicId|lists:delete(TopicId,Subs)]}),
  {ok,TopicId}.


-spec create_topic(Url :: binary(), Options :: list, State :: #state{}) -> {ok,#topic{}}.
create_topic(Url,Options,#state{t=Topics,ut=UrlTopic}=State) ->
  Id = gen_id(),
  Topic =
    case ets:lookup(Topics,Id) of
      [] ->
        T = #topic{id=Id,url=Url,options=Options},
        true = ets:insert_new(Topics,T),
        true = ets:insert_new(UrlTopic,{Url,Id}),
        T;
      _ -> create_topic(Url,Options,State)
    end,
  {ok,Topic}.


-spec unsubscribe_from_topic(Pid :: pid(), SubscriptionId :: non_neg_integer(), State :: #state{}) -> {ok} | {error,no_such_subscription,list()}.
unsubscribe_from_topic(Pid,SubscriptionId,State) ->
  Session = get_session_from_pid(Pid,State),

  case remove_session_from_topic(Session,SubscriptionId,State) of
    ok -> {ok};
    _ -> {error,[{}],no_such_subscription}
  end.


-spec send_event_to_topic(FromPid :: pid(), Options :: list(), Url :: binary(), Arguments :: list()|undefined, ArgumentsKw :: list()|undefined, State :: #state{} ) -> {ok,non_neg_integer()}.
send_event_to_topic(FromPid,_Options,Url,Arguments,ArgumentsKw,#state{sess=Sessions,ut=UT,t=T}) ->
  PublicationId =
    case ets:lookup(UT,Url) of
      [] ->
        gen_id();
      [{Url,TopicId}] ->
        [Topic] = ets:lookup(T,TopicId),
        IdToPid = fun(Id,Pids) -> [#session{pid=Pid}] = ets:lookup(Sessions,Id), [Pid|Pids] end,
        Peers = lists:delete(FromPid,lists:foldl(IdToPid,[],Topic#topic.subscribers)),
        SubscriptionId = Topic#topic.id,
        PublishId = gen_id(),
        Details = [{}],
        Message = {event,SubscriptionId,PublishId,Details,Arguments,ArgumentsKw},
        send_message_to_peers(Message,Peers),
        PublishId
    end,
  {ok,PublicationId}.


-spec register_procedure(Pid :: pid(), Options :: list(), ProcedureUrl :: binary(), State :: #state{}) -> {ok,non_neg_integer()} | {error,list(),procedure_already_exists }.
register_procedure(Pid,Options,ProcedureUrl,#state{up=UP}=State) ->
  Session = get_session_from_pid(Pid,State),

  case ets:lookup(UP,ProcedureUrl) of
    [] ->
      create_procedure(ProcedureUrl,Options,Session,State);
    _ ->
      {error,[{}],procedure_already_exists}
  end  .


-spec unregister_procedure( Pid :: pid(), ProcedureId :: non_neg_integer(), State :: #state{}) -> {ok} | {error,Details :: list(), Reason :: atom()}.
unregister_procedure(Pid,ProcedureId,State) ->
  Session = get_session_from_pid(Pid,State),
  case remove_session_from_procedure(Session,ProcedureId,State) of
    ok -> {ok};
    not_found -> {error,[{}],no_such_registration}
  end.


enqueue_procedure_call(Pid, RequestId, Options,ProcedureUrl,Arguments,ArgumentsKw,#state{up=UP,p=P,sess=Sessions}=State) ->
  Session = get_session_from_pid(Pid,State),
  %SessionId = Session#session.id,

  case ets:lookup(UP,ProcedureUrl) of
    [] ->
      {error,[{}],no_such_procedure};
    [{ProcedureUrl,ProcId}] ->
      [Procedure] = ets:lookup(P,ProcId),
      ProcedureId = Procedure#procedure.id,
      CalleeId = Procedure#procedure.session_id,
      [CalleeSession] = ets:lookup(Sessions,CalleeId),
      CalleePid = CalleeSession#session.pid,

      Details = [{}],
      {ok,InvocationId} = create_invocation(Session,CalleeSession,RequestId,Procedure,Options,Arguments,ArgumentsKw,State),
      send_message_to_peers({invocation,InvocationId,ProcedureId,Details,Arguments,ArgumentsKw},[CalleePid]),
      {ok}
  end.

dequeue_procedure_call(Pid,Id,_Options,Arguments,ArgumentsKw,#state{i=I,sess=Sessions}=State) ->
  Session = get_session_from_pid(Pid,State),
  SessionId = Session#session.id,
  case ets:lookup(I,Id) of
    [] -> {error,not_found};
    [Invocation] ->
      case Invocation#invocation.callee_id of
       SessionId ->
          #invocation{caller_id=CallerId, request_id=RequestId} = Invocation,
          Details = [{}],
          [Caller] = ets:lookup(Sessions,CallerId),
          send_message_to_peers({result,RequestId,Details,Arguments,ArgumentsKw},[Caller#session.pid]),
          remove_invocation(Id,State),
          {ok};
        _ ->
          {error,wrong_session}
      end
  end.

-spec remove_session_with_ref(Ref :: reference(), State :: #state{}) -> ok.
remove_session_with_ref(Ref,#state{sess=Sessions}=State) ->
  [RefSession] = ets:lookup(Sessions,Ref),
  Id = RefSession#ref_session.session_id,
  [Session] = ets:lookup(Sessions,Id),

  RemoveTopic = fun(TopicId,Results) ->
        Result = remove_session_from_topic(Session,TopicId,State),
        [{TopicId,Result}|Results]
        end,
  _ResultTopics = lists:foldl(RemoveTopic,[],Session#session.subscriptions),

  RemoveRegistration = fun(RegistrationId,Results) ->
        Result = remove_session_from_procedure(Session,RegistrationId,State),
        [{RegistrationId,Result}|Results]
        end,
  _ResultRegistrations = lists:foldl(RemoveRegistration,[],Session#session.registrations),

  ets:delete(Sessions,Id),
  ets:delete(Sessions,Ref),
  ets:delete(Sessions,Session#session.pid),
  ok.



-spec remove_session_from_topic(Session :: #session{}, TopicId :: non_neg_integer(), State :: #state{}) -> ok | not_found.
remove_session_from_topic(Session,TopicId,#state{t=T,sess=Sessions}) ->
  SessionId = Session#session.id,
  [Topic] = ets:lookup(T,TopicId),

  case lists:member(TopicId,Session#session.subscriptions) of
    false ->
      not_found;

    true ->
      ets:update_element(T,TopicId,{#topic.subscribers,lists:delete(SessionId,Topic#topic.subscribers)}),
      ets:update_element(Sessions,SessionId,{#session.subscriptions,lists:delete(TopicId,Session#session.subscriptions)}),
      ok
  end.


-spec create_procedure(Url :: binary(), Options :: list(), Session :: #session{}, State :: #state{} ) -> {ok,non_neg_integer()}.
create_procedure(Url,Options,Session,#state{p=P,up=UP,sess=Sessions}=State) ->
  SessionId = Session#session.id,
  ProcedureId = gen_id(),
  case ets:lookup(P,ProcedureId) of
    [] ->
      true = ets:insert_new(P,#procedure{id=ProcedureId,url=Url,session_id=SessionId,options=Options}),
      true = ets:insert_new(UP,{Url,ProcedureId}),
      true = ets:update_element(Sessions,SessionId,{#session.registrations, [ProcedureId| Session#session.registrations]}),
      {ok,ProcedureId};
    _ ->
      create_procedure(Url,Options,Session,State)
  end.

-spec remove_session_from_procedure( Session :: #session{}, ProcedureId :: non_neg_integer(), State :: #state{}) -> ok | not_found.
remove_session_from_procedure(Session,ProcedureId,#state{p=P,sess=Sessions}) ->
  SessionId = Session#session.id,

  case lists:member(ProcedureId,Session#session.registrations) of
    false ->
      not_found;

    true ->
      ets:delete(P,ProcedureId),
      ets:update_element(Sessions,SessionId,{#session.registrations,lists:delete(ProcedureId,Session#session.registrations)}),
      ok
  end.

-spec create_invocation(Session :: #session{}, CalleeSession :: #session{}, RequestId :: non_neg_integer(), Procedure :: #procedure{}, Options :: list(), Arguments :: list(), ArgumentsKw :: list(), State :: #state{}) -> {ok, non_neg_integer()}.
create_invocation(Session,CalleeSession,RequestId,Procedure,Options,Arguments,ArgumentsKw,#state{i=I}=State) ->
  Id = gen_id(),
  case ets:lookup(I,Id) of
    [] ->
      Invocation = #invocation{
        timestamp = undefined,
        id = Id,
        procedure_id = Procedure#procedure.id,
        request_id = RequestId,
        caller_id = Session#session.id,
        callee_id = CalleeSession#session.id,
        options = Options,
        arguments = Arguments,
        argumentskw = ArgumentsKw },
        true = ets:insert_new(I,Invocation),
      {ok,Id};
    _ ->
      create_invocation(Session,CalleeSession,RequestId,Procedure,Options,Arguments,ArgumentsKw,State)
  end.

-spec remove_invocation(Id :: non_neg_integer(), State :: #state{}) -> ok.
remove_invocation(InvocationId,#state{i=I}) ->
  true = ets:delete(I,InvocationId),
  ok.


-spec send_message_to_peers(Msg :: term(), Peers :: list()) -> ok.
send_message_to_peers(Msg,Peers) ->
  Send = fun(Pid) -> Pid ! {erwa,Msg} end,
  lists:foreach(Send,Peers),
  ok.


-spec get_session_from_pid(Pid :: pid(), State :: #state{}) -> #session{}.
get_session_from_pid(Pid,#state{sess=Sessions}) ->
  [PidSession] = ets:lookup(Sessions,Pid),
  [Session] = ets:lookup(Sessions,PidSession#pid_session.session_id),
  Session.


-spec gen_id() -> non_neg_integer().
gen_id() ->
  crypto:rand_uniform(0,9007199254740993).



-ifdef(TEST).

hello_welcome_test() ->
  {ok,Pid} = start(<<"some.realm">>),
  {welcome,_,_} = hello(Pid,[]),
  shutdown = hello(Pid,[]).


 subscribe_test() ->
   {ok,Pid} = start(<<"some.realm">>),
   {welcome,_,_} = hello(Pid,[]),
   RequestId = crypto:rand_uniform(0,9007199254740993),
   {subscribed,RequestId,_SubscriptionId} = subscribe(Pid,RequestId,[],<<"does.not.exist">>).


resubscribe_test() ->
  {ok,Pid} = start(<<"some.realm">>),
  {welcome,_,_} = hello(Pid,[]),
  RequestId = crypto:rand_uniform(0,9007199254740993),
  {subscribed,RequestId,SubscriptionId} = subscribe(Pid,RequestId,[],<<"does.not.exist">>),
  {unsubscribed,RequestId} = unsubscribe(Pid,RequestId,SubscriptionId),
  {error,unsubscribe,RequestId,_Details,no_such_subscription} = unsubscribe(Pid,RequestId,SubscriptionId),
  RequestId2 = crypto:rand_uniform(0,9007199254740993),
  {subscribed,RequestId2,SubscriptionId} = subscribe(Pid,RequestId2,[],<<"does.not.exist">>).


register_test() ->
  {ok,Pid} = start(<<"some.realm">>),
  {welcome,_,_} = hello(Pid,<<"blah">>),
  RequestId = crypto:rand_uniform(0,9007199254740993),
  {registered,RequestId,_RegistrationId} = register(Pid,RequestId,[],<<"nice_fun">>).

unregister_test() ->
  {ok,Pid} = start(<<"some.realm">>),
  {welcome,_,_} = hello(Pid,<<"blah">>),
  RequestId = crypto:rand_uniform(0,9007199254740993),
  {registered,RequestId,RegistrationId} = register(Pid,RequestId,[],<<"nice_fun">>),
  {unregistered,RequestId} = unregister(Pid,RequestId,RegistrationId),
  {error,unregister,RequestId,_Details,no_such_registration} = unregister(Pid,RequestId,RegistrationId).


%call_test() ->


-endif.

