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

-module(erwa_broker).
-behaviour(gen_server).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



%% API
-export([subscribe/4]).
-export([unsubscribe/2]).
-export([unsubscribe_all/1]).
-export([publish/6]).

-export([get_features/1]).

-export([enable_metaevents/1]).
-export([disable_metaevents/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


%% internal
-export([start/0]).
-export([start_link/0]).
-export([stop/1]).

-export([get_data/1]).

-define(FEATURES,#{features => #{
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


-record(data, {
               ets = none,
               pid = unknown,
               features = ?FEATURES
               }).

-record(state, {
                ets = none,
                meta_events = enabled
                }).


-record(topic, {
                uri = unknown,
                id = none,
                match = exact,
                created = unknown,
                subscribers = []}).

-record(id_topic, {
                   id = none,
                   topic = unknown
                   }).

-record(pid_info, {
                     pid = none,
                     id = unknown,
                     topics = []
                     }).

start() ->
  gen_server:start(?MODULE, [], []).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec get_data( pid() ) -> {ok, record(data)}.
get_data(Pid) ->
   gen_server:call(Pid, get_data).


-spec enable_metaevents( pid() ) -> ok.
enable_metaevents( Pid ) ->
  gen_server:call(Pid,enable_metaevents).

-spec disable_metaevents( pid() ) -> ok.
disable_metaevents( Pid ) ->
  gen_server:call(Pid,disable_metaevents).


-spec subscribe(Topic::binary(),Options::map(), Session :: term(), Data::record(data)) -> {ok, non_neg_integer()}.
subscribe(Topic,Options,Session,#data{pid=Pid}) ->
  gen_server:call(Pid, {subscribe,Topic,Options,Session} ).

-spec unsubscribe(SubscriptionId::non_neg_integer(), Data::record(data)) -> ok | {error, Reason::term()}.
unsubscribe(SubscriptionId,#data{pid=Pid}) ->
  gen_server:call(Pid, {unsubscribe,SubscriptionId} ).

-spec unsubscribe_all(Data::record(data)) -> ok.
unsubscribe_all(#data{pid=Pid}) ->
  gen_server:call(Pid,unsubscribe_all).

-spec publish(Topic::binary(),Options::map(),Arguments :: list(), ArgumentsKw :: map(), Session :: term(), Data::record(data)) ->
  {ok, non_neg_integer()}.

publish(TopicUri,Options,Arguments,ArgumentsKw,Session,#data{ets=Ets}) ->
  case ets:lookup(Ets,TopicUri) of
    [#topic{subscribers=Subs,id=SubscriptionId}] ->
      {ok,PublicationID} = erwa_publications:get_pub_id(),
      Receipients = case maps:get(exclude_me,Options,true) of
                      false ->
                        Subs;
                      _ ->
                        lists:delete(self(),Subs)
                    end,
      Details = case maps:get(disclose_me,Options,false) of
                  true -> #{publisher => erwa_session:get_id(Session)};
                  _ -> #{}
                end,

      ToExclude = maps:get(exclude,Options,[]),
      ToEligible = maps:get(eligible,Options,Subs),
      SendFilter = fun(Pid) ->
                 case ets:lookup(Ets,Pid) of
                   [#pid_info{id=SessionId}] ->
                     case (not lists:member(SessionId,ToExclude)) and
                       (lists:member(SessionId,ToEligible) or lists:member(Pid,ToEligible)) of
                       true ->
                         Pid ! {erwa,{event,SubscriptionId,PublicationID,Details,Arguments,ArgumentsKw}},
                         true;
                       false ->
                         false
                     end;
                   _ ->
                     false
                 end
               end,
      lists:filter(SendFilter,Receipients),
      {ok,PublicationID};
    [] ->
      {ok,gen_id()}
  end.

-spec get_features(record(data)) -> term().
get_features(#data{features = F}) ->
  F.

stop(#data{pid=Pid}) ->
  stop(Pid);
stop(Pid) ->
  gen_server:call(Pid, stop).

  %% gen_server.

init([]) ->
  Ets = ets:new(events,[set,{keypos,2}]),
	{ok, #state{ets=Ets}}.


handle_call({subscribe,TopicUri,Options,Session}, {Pid, _Ref}, State) ->
  Result = subscribe(Pid,TopicUri,Options,Session,State),
	{reply,Result,State};
handle_call({unsubscribe,SubscriptionId}, {Pid, _Ref}, State) ->
  Result = unsubscribe(SubscriptionId,Pid,State),
	{reply,Result,State};
handle_call(unsubscribe_all, {Pid, _Ref}, #state{ets=_Ets} = State) ->
  Result = unsubscribe_all_for(Pid,State),
	{reply,Result,State};
handle_call(get_data, _From, #state{ets=Ets} = State) ->
	{reply,{ok,#data{ets=Ets, pid=self()}},State};
handle_call(enable_metaevents, _From, State) ->
  {reply,ok,State#state{meta_events=enabled}};
handle_call(disable_metaevents, _From, State) ->
  {reply,ok,State#state{meta_events=disabled}};
handle_call(stop, _From, State) ->
	{stop,normal,{ok,stopped},State};
handle_call(_Request, _sFrom, State) ->
	{reply, ignored, State}.


handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



-spec subscribe(Pid:: pid(),TopicUri :: binary(), Options :: map(), Session::term(), State :: record(state)) ->
  {ok, ID::non_neg_integer()} | {error, Reason :: term()}.
subscribe(Pid,TopicUri,Options,Session,#state{ets=Ets}=State) ->
  Match = maps:get(match,Options,exact),
  case Match of
    prefix ->
      {error,not_supported};
    wildcard ->
      {error,not_supported};
    exact ->
      {SubscriptionId,Created,TopicDetails} = case ets:lookup(Ets,TopicUri) of
                                                [#topic{id=SID,subscribers=Subs}=T] ->
                                                  NewSubs = [Pid|lists:delete(Pid,Subs)],
                                                  ets:insert(Ets,T#topic{subscribers=NewSubs}),
                                                  {SID,false,not_needed};
                                                [] ->
                                                  {ok,SID,TDetails} = create_topic(TopicUri,Match,[Pid],State),
                                                  {SID,true,TDetails}
                                              end,
      ok = add_topic_to_pid(TopicUri,Pid,Session,State),
      SessionId = erwa_session:get_id(Session),
      case Created of
        true ->
          publish_metaevent(on_create,TopicUri,SessionId,TopicDetails,State);
        false -> nothing
      end,
      publish_metaevent(on_subscribe,TopicUri,SessionId,SubscriptionId,State),
      {ok,SubscriptionId}
  end.

-spec unsubscribe(IdOrTopic :: non_neg_integer() | binary(), Pid :: pid(), State :: record(state)) ->
  ok | {error, Reason :: term()}.

unsubscribe(SubscriptionId,Pid,#state{ets=Ets}=State) when is_integer(SubscriptionId) ->
  case ets:lookup(Ets,SubscriptionId) of
    [#id_topic{id=SubscriptionId,topic=TopicUri}] ->
      unsubscribe(TopicUri,Pid,State);
    [] ->
      {error,not_found}
  end;
unsubscribe(TopicUri,Pid,#state{ets=Ets}=State) when is_binary(TopicUri) ->
  [#topic{subscribers=Subs,id=SubscriptionId,uri=TopicUri}=T] = ets:lookup(Ets,TopicUri),
  SessionId = get_id_for_pid(Pid,State),
  case {lists:member(Pid,Subs),lists:delete(Pid,Subs)} of
    {false,_} ->
      {error, not_subscribed};
    {true,[]} ->
      ok = remove_topic_from_pid(TopicUri,Pid,State),
      true = ets:delete(Ets,SubscriptionId),
      true = ets:delete(Ets,TopicUri),
      publish_metaevent(on_unsubscribe,TopicUri,SessionId,SubscriptionId,State),
      publish_metaevent(on_delete,TopicUri,SessionId,SubscriptionId,State),
      ok;
    {true,NewSubs} ->
      ok = remove_topic_from_pid(TopicUri,Pid,State),
      true = ets:insert(Ets,T#topic{subscribers=NewSubs}),
      publish_metaevent(on_unsubscribe,TopicUri,SessionId,SubscriptionId,State),
      ok
  end.


-spec unsubscribe_all_for( Pid :: pid(), State :: record(state)) ->
  ok | {error, Reason :: term()}.
unsubscribe_all_for(Pid,#state{ets=Ets}=State) ->
  case ets:lookup(Ets,Pid) of
    [#pid_info{topics=Topics,pid=Pid}] ->
      F = fun(TopicUri) ->
            ok = unsubscribe(TopicUri,Pid,State),
            false
          end,
      lists:filter(F,Topics),
      ok;
    [] ->
      ok
  end.

create_topic(Uri,Match,Pids,#state{ets=Ets}=State) ->
  ID = gen_id(),
  Created = erlang:universaltime(),
  Topic = #topic{uri=Uri,id=ID,match=Match,created=Created,subscribers=Pids},
  case ets:insert_new(Ets,[#id_topic{id=ID,topic=Uri},Topic]) of
    true ->
      {ok,ID,#{id => ID,
               created => cowboy_clock:rfc1123(Created),
               uri => Uri,
               match => Match}};
    false ->
      create_topic(Uri,Match,Pids,State)
  end.

add_topic_to_pid(Topic,Pid,Session,#state{ets=Ets}) ->
  SessionId = erwa_session:get_id(Session),
  case ets:lookup(Ets,Pid) of
    [#pid_info{topics=Topics} = PT] ->
      true = ets:insert(Ets,PT#pid_info{topics=[Topic|lists:delete(Topic,Topics)]}),
      ok;
    [] ->
      PT = #pid_info{pid=Pid,id=SessionId,topics=[Topic]},
      true = ets:insert_new(Ets,PT),
      ok
  end.


remove_topic_from_pid(Topic,Pid,#state{ets=Ets}) ->
  [#pid_info{topics=Topics} = PT] = ets:lookup(Ets,Pid),
  case lists:delete(Topic,Topics) of
    [] ->
      true = ets:delete(Ets,Pid);
    NewTopics ->
      true = ets:insert(Ets,PT#pid_info{topics=NewTopics})
  end,
  ok.

get_id_for_pid(Pid,#state{ets=Ets}) ->
  [#pid_info{id=SessionId}] = ets:lookup(Ets,Pid),
  SessionId.


gen_id() ->
  crypto:rand_uniform(0,9007199254740992).

publish_metaevent(_,_,_,_,#state{meta_events=disabled}) ->
  ok;
publish_metaevent(Event,TopicUri,SessionId,SecondArg,#state{ets=Ets}) ->
  case binary:part(TopicUri,{1,5}) == <<"wamp.">> of
    true ->
      % do not fire metaevents on wamp. uris
      ok;
    false ->
      MetaTopic = case Event of
                    on_create -> <<"wamp.subscription.on_create">>;
                    on_subscribe -> <<"wamp.subscription.on_subscribe">>;
                    on_unsubscribe -> <<"wamp.subscription.on_unsubscribe">>;
                    on_delete -> <<"wamp.subscription.on_delete">>
                  end,
      {ok,_} = publish(MetaTopic,#{},[SessionId,SecondArg],undefined,no_session,#data{ets=Ets})
  end,
  ok.


-ifdef(TEST).

get_tablesize(#data{ets=Ets}) ->
  ets:info(Ets,size).

start_stop_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  0 = get_tablesize(Data),
  ok = enable_metaevents(Pid),
  {ok,stopped} = stop(Data).

un_subscribe_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  ok = disable_metaevents(Pid),
  Session = erwa_session:create(),
  0 = get_tablesize(Data),
  {ok,ID1} = subscribe(<<"topic.test1">>,#{},Session,Data),
  3 = get_tablesize(Data),
  {ok,ID2} = subscribe(<<"topic.test2">>,#{},Session,Data),
  5 = get_tablesize(Data),
  ok = unsubscribe(ID1,Data),
  3 = get_tablesize(Data),
  {error,not_found} = unsubscribe(ID1,Data),
  ok = unsubscribe(ID2,Data),
  0 = get_tablesize(Data),
  {error,not_found} = unsubscribe(ID2,Data),
  0 = get_tablesize(Data),
  {ok,stopped} = stop(Data).

unsubscribe_all_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  ok = disable_metaevents(Pid),
  Session = erwa_session:create(),
  0 = get_tablesize(Data),
  ok = unsubscribe_all(Data),
  0 = get_tablesize(Data),
  {ok,ID1} = subscribe(<<"topic.test1">>,#{},Session,Data),
  3 = get_tablesize(Data),
  {ok,ID2} = subscribe(<<"topic.test2">>,#{},Session,Data),
  5 = get_tablesize(Data),
  ok = unsubscribe_all(Data),
  0 = get_tablesize(Data),
  {error,not_found} = unsubscribe(ID1,Data),
  0 = get_tablesize(Data),
  {error,not_found} = unsubscribe(ID2,Data),
  0 = get_tablesize(Data),
  ok = unsubscribe_all(Data),
  0 = get_tablesize(Data),
  {ok,stopped} = stop(Data).


multiple_un_subscribe_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  ok = disable_metaevents(Pid),
  Session = erwa_session:create(),
  0 = get_tablesize(Data),
  {ok,ID1} = subscribe(<<"topic.test1">>,#{},Session,Data),
  3 = get_tablesize(Data),
  {ok,ID2} = subscribe(<<"topic.test2">>,#{},Session,Data),
  5 = get_tablesize(Data),
  MyPid = self(),
  F = fun() ->
        S2 = erwa_session:create(),
        {ok,ID3} = erwa_broker:subscribe(<<"topic.test1">>,#{},S2,Data),
        MyPid ! {first_subscription,ID3},
        receive
        after 200 -> ok
        end,
        {ok,ID4} = erwa_broker:subscribe(<<"topic.test2">>,#{},S2,Data),
        MyPid ! {second_subscription,ID4},
        receive
        after 200 -> ok
        end,
        ok = erwa_broker:unsubscribe_all(Data),
        MyPid ! done,
        ok
      end,
  spawn(F),
  receive
    {first_subscription,ID1} ->
      ok
  end,
  6 = get_tablesize(Data),
  receive
    {second_subscription,ID2} ->
      ok
  end,
  6 = get_tablesize(Data),
  receive
    done ->
      ok
  end,
  5 = get_tablesize(Data),
  ok = unsubscribe(ID1,Data),
  3 = get_tablesize(Data),
  ok = unsubscribe_all(Data),
  0 = get_tablesize(Data),
  {ok,stopped} = stop(Data).


publish_test() ->
  {ok,_} = erwa_publications:start(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  ok = disable_metaevents(Pid),
  Session = erwa_session:create(),
  {ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},Session,Data),
  Session = erwa_session:create(),
  MyPid = self(),
  F = fun() ->
        S2 = erwa_session:create(),
        {ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},S2,Data),
        MyPid ! subscribed,
        receive
          {erwa,{event,ID,PubId,#{},undefined,undefined}} ->
            MyPid ! {received,PubId}
        end,
        ok = erwa_broker:unsubscribe_all(Data),
        ok
      end,
  spawn(F),
  receive
    subscribed -> ok
  end,
  {ok,PublicationID1} = publish(<<"topic.test1">>,#{},undefined,undefined,Session,Data),
  receive
    {received,PublicationID1} -> ok
  end,
  {ok,PublicationID2} = publish(<<"topic.test1">>,#{exclude_me=>false},undefined,undefined,Session,Data),
  ok = receive
         {erwa,{event,ID,PublicationID2,#{},undefined,undefined}} ->
           ok
       end,
  {ok,stopped} = stop(Data),
  {ok,stopped} = erwa_publications:stop().


exclude_test() ->
  {ok,_} = erwa_publications:start(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  ok = disable_metaevents(Pid),
  SessionId1 = gen_id(),
  SessionId2 = gen_id(),
  Session = erwa_session:set_id(SessionId1,erwa_session:create()),
  {ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},Session,Data),
  MyPid = self(),
  F = fun() ->
        S2 = erwa_session:set_id(SessionId2,erwa_session:create()),
        {ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},S2,Data),
        MyPid ! subscribed,
        Received  = receive
                      {erwa,{event,ID,_,#{},undefined,undefined}} ->
                        true;
                      got_something ->
                        MyPid ! nothing,
                        false
                    end,
        case Received of
          true ->
            receive
              {got_something} ->
                MyPid ! yes_got_it
            end;
          false ->
            ok
        end,
        ok = erwa_broker:unsubscribe_all(Data),
        MyPid ! done,
        ok
      end,
  ClientPid = spawn(F),
  receive
    subscribed -> ok
  end,
  {ok,PubID} = publish(<<"topic.test1">>,#{exclude_me => false,exclude => [SessionId2]},undefined,undefined,Session,Data),
  ok = receive
         {erwa,{event,ID,PubID,#{},undefined,undefined}} ->
           ok
       end,
  receive
    after 100 -> ok
  end,
  ClientPid ! got_something,
  ok = receive
         nothing -> ok;
         yes_got_it -> wrong
       end,
  {ok,stopped} = stop(Data),
  {ok,stopped} = erwa_publications:stop().



eligible_test() ->
  {ok,_} = erwa_publications:start(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  ok = disable_metaevents(Pid),
  SessionId1 = gen_id(),
  SessionId2 = gen_id(),
  Session = erwa_session:set_id(SessionId1,erwa_session:create()),
  {ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},Session,Data),
  MyPid = self(),
  F = fun() ->
        S2 = erwa_session:set_id(SessionId2,erwa_session:create()),
        {ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},S2,Data),
        MyPid ! subscribed,
        Received  = receive
                      {erwa,{event,ID,_,[],undefined,undefined}} ->
                        true;
                      got_something ->
                        MyPid ! nothing,
                        false
                    end,
        case Received of
          true ->
            receive
              {got_something} ->
                MyPid ! yes_got_it
            end;
          false ->
            ok
        end,
        ok = erwa_broker:unsubscribe_all(Data),
        MyPid ! done,
        ok
      end,
  ClientPid = spawn(F),
  receive
    subscribed -> ok
  end,
  {ok,PubID} = publish(<<"topic.test1">>,#{exclude_me=>false,eligible=>[SessionId1]},undefined,undefined,Session,Data),
  ok = receive
         {erwa,{event,ID,PubID,#{},undefined,undefined}} ->
           ok
       end,
  receive
    after 100 -> ok
  end,
  ClientPid ! got_something,
  ok = receive
         nothing -> ok;
         yes_got_it -> wrong
       end,
  {ok,stopped} = stop(Data),
  {ok,stopped} = erwa_publications:stop().


garbage_test() ->
  {ok,Pid} = start(),
  ignored = gen_server:call(Pid,some_garbage),
  ok = gen_server:cast(Pid,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop(Pid).


-endif.
