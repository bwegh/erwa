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

-include("erwa_model.hrl").

%% API
-export([subscribe/4]).
-export([unsubscribe/3]).
-export([unsubscribe_all/2]).
-export([publish/6]).

-export([get_features/1]).

-export([enable_metaevents/1]).
-export([disable_metaevents/1]).

-export([get_subscriptions/1]).
-export([get_subscription/2]).

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

-record(state,
{
  ets = none,
  meta_events = enabled
}).


start() ->
  gen_server:start(?MODULE, [], []).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec get_data(pid()) -> {ok, #data{}}.
get_data(Pid) ->
  gen_server:call(Pid, get_data).

-spec enable_metaevents(#data{}) -> ok.
enable_metaevents(#data{pid = Pid}) ->
  gen_server:call(Pid, enable_metaevents).

-spec disable_metaevents(#data{}) -> ok.
disable_metaevents(#data{pid = Pid}) ->
  gen_server:call(Pid, disable_metaevents).

-spec get_subscriptions(#data{}) -> map().
get_subscriptions(#data{pid = Pid}) ->
  gen_server:call(Pid, get_subscriptions).

-spec get_subscription(#data{}, non_neg_integer()) -> map().
get_subscription(#data{pid = Pid}, SubscriptionId) ->
  gen_server:call(Pid, {get_subscription, SubscriptionId}).

-spec subscribe(Topic :: binary(), Options :: map(), SessionId :: non_neg_integer(), Data :: #data{}) -> {ok, non_neg_integer()}.
subscribe(Topic, Options, SessionId, #data{pid = Pid}) ->
  gen_server:call(Pid, {subscribe, Topic, Options, SessionId}).

-spec unsubscribe(SubscriptionId :: non_neg_integer(), SessionId :: non_neg_integer(), Data :: #data{}) -> ok | {error, Reason :: term()}.
unsubscribe(SubscriptionId, SessionId, #data{pid = Pid}) ->
  gen_server:call(Pid, {unsubscribe, SubscriptionId, SessionId}).

-spec unsubscribe_all(SessionId :: non_neg_integer(), Data :: #data{}) -> ok.
unsubscribe_all(SessionId, #data{pid = Pid}) ->
  gen_server:call(Pid, {unsubscribe_all, SessionId}).

-spec publish(Topic :: binary(), Options :: map(), Arguments :: list(), ArgumentsKw :: map(), Session :: term(), Data :: #data{}) ->
  {ok, non_neg_integer()}.

publish(TopicUri, Options, Arguments, ArgumentsKw, SessionId, #data{ets = Ets}) ->
  case ets:lookup(Ets, TopicUri) of
    [#topic{subscribers = Subs, id = SubscriptionId}] ->
      {ok, PublicationID} = erwa_publications:get_pub_id(),
      Receipients = case maps:get(exclude_me, Options, true) of
                      false ->
                        Subs;
                      _ ->
                        lists:delete(SessionId, Subs)
                    end,
      Details = case maps:get(disclose_me, Options, false) of
                  true -> #{publisher => SessionId};
                  _ -> #{}
                end,

      ToExclude = maps:get(exclude, Options, []),
      ToEligible = maps:get(eligible, Options, Receipients),
      SendFilter =
        fun(SessId) ->
          case (not lists:member(SessId, ToExclude)) and lists:member(SessId, ToEligible) of
            true ->
              Msg = {event, SubscriptionId, PublicationID, Details, Arguments, ArgumentsKw},
              erwa_sessions:send_message_to(Msg, SessId),
              true;
            false ->
              false
          end
        end,
      lists:filter(SendFilter, Receipients),
      {ok, PublicationID};
    [] ->
      {ok, erwa_support:gen_id()}
  end.

-spec get_features(#data{}) -> term().
get_features(#data{features = F}) ->
  F.

stop(#data{pid = Pid}) ->
  stop(Pid);
stop(Pid) ->
  gen_server:call(Pid, stop).

%% gen_server.

init([]) ->
  Ets = ets:new(events, [set, {keypos, 2}]),
  {ok, #state{ets = Ets}}.


handle_call({subscribe, TopicUri, Options, SessionId}, _From, State) ->
  Result = int_subscribe(TopicUri, Options, SessionId, State),
  {reply, Result, State};
handle_call({unsubscribe, SubscriptionId, SessionId}, _From, State) ->
  Result = int_unsubscribe(SubscriptionId, SessionId, State),
  {reply, Result, State};
handle_call({unsubscribe_all, SessionId}, _From, #state{ets = _Ets} = State) ->
  Result = unsubscribe_all_for(SessionId, State),
  {reply, Result, State};
handle_call(get_data, _From, #state{ets = Ets} = State) ->
  {reply, {ok, #data{ets = Ets, pid = self(), features = ?BROKER_FEATURES}}, State};
handle_call(get_subscriptions, _From, #state{ets = Ets} = State) ->
  Exact = lists:flatten(ets:match(Ets, #topic{match = exact, id = '$1', _ = '_'})),
  Prefix = lists:flatten(ets:match(Ets, #topic{match = prefix, id = '$1', _ = '_'})),
  Wildcard = lists:flatten(ets:match(Ets, #topic{match = wildcard, id = '$1', _ = '_'})),
  {reply, {ok, #{exact => Exact, prefix => Prefix, wildcard => Wildcard}}, State};
handle_call({get_subscription, SubscriptionId}, _From, #state{ets = Ets} = State) ->
  case ets:lookup(Ets, SubscriptionId) of
    [#id_topic{id = SubscriptionId, topic = Uri}] ->
      [#topic{uri = Uri, match = Match, created = Created, id = Id}] = ets:lookup(Ets, Uri),
      {reply, {ok, #{uri => Uri, id => Id, match => Match, created => Created}}, State};
    [] ->
      {reply, {error, not_found}, State}
  end;
handle_call(enable_metaevents, _From, State) ->
  {reply, ok, State#state{meta_events = enabled}};
handle_call(disable_metaevents, _From, State) ->
  {reply, ok, State#state{meta_events = disabled}};
handle_call(stop, _From, State) ->
  {stop, normal, {ok, stopped}, State};
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


%% @private
-spec int_subscribe(TopicUri :: binary(), Options :: map(), SessionId :: non_neg_integer(), State :: #state{}) ->
  {ok, ID :: non_neg_integer()} | {error, Reason :: term()}.
int_subscribe(TopicUri, Options, SessionId, #state{ets = Ets} = State) ->
  Match = maps:get(match, Options, exact),
  case Match of
    prefix ->
      {error, not_supported};
    wildcard ->
      {error, not_supported};
    exact ->
      {SubscriptionId, Created, TopicDetails} = case ets:lookup(Ets, TopicUri) of
                                                  [#topic{id = SID, subscribers = Subs} = T] ->
                                                    NewSubs = [SessionId | lists:delete(SessionId, Subs)],
                                                    ets:insert(Ets, T#topic{subscribers = NewSubs}),
                                                    {SID, false, not_needed};
                                                  [] ->
                                                    {ok, SID, TDetails} = create_topic(TopicUri, Match, [SessionId], State),
                                                    {SID, true, TDetails}
                                                end,
      ok = add_topic_to_session(TopicUri, SessionId, State),
      case Created of
        true ->
          publish_metaevent(on_create, TopicUri, SessionId, TopicDetails, State);
        false -> nothing
      end,
      publish_metaevent(on_subscribe, TopicUri, SessionId, SubscriptionId, State),
      {ok, SubscriptionId}
  end.

%% @private
-spec int_unsubscribe(IdOrTopic :: non_neg_integer() | binary(), SessionId :: non_neg_integer(), State :: #state{}) ->
  ok | {error, Reason :: term()}.
int_unsubscribe(SubscriptionId, SessionId, #state{ets = Ets} = State) when is_integer(SubscriptionId) ->
  case ets:lookup(Ets, SubscriptionId) of
    [#id_topic{id = SubscriptionId, topic = TopicUri}] ->
      int_unsubscribe(TopicUri, SessionId, State);
    [] ->
      {error, not_found}
  end;
int_unsubscribe(TopicUri, SessionId, #state{ets = Ets} = State) when is_binary(TopicUri) ->
  [#topic{subscribers = Subs, id = SubscriptionId, uri = TopicUri} = T] = ets:lookup(Ets, TopicUri),
  case {lists:member(SessionId, Subs), lists:delete(SessionId, Subs)} of
    {false, _} ->
      {error, not_subscribed};
    {true, []} ->
      ok = remove_topic_from_session(TopicUri, SessionId, State),
      true = ets:delete(Ets, SubscriptionId),
      true = ets:delete(Ets, TopicUri),
      publish_metaevent(on_unsubscribe, TopicUri, SessionId, SubscriptionId, State),
      publish_metaevent(on_delete, TopicUri, SessionId, SubscriptionId, State),
      ok;
    {true, NewSubs} ->
      ok = remove_topic_from_session(TopicUri, SessionId, State),
      true = ets:insert(Ets, T#topic{subscribers = NewSubs}),
      publish_metaevent(on_unsubscribe, TopicUri, SessionId, SubscriptionId, State),
      ok
  end.

%% @private
-spec unsubscribe_all_for(SessionId :: non_neg_integer(), State :: #state{}) ->
  ok | {error, Reason :: term()}.
unsubscribe_all_for(SessionId, #state{ets = Ets} = State) ->
  case ets:lookup(Ets, {sess, SessionId}) of
    [#id_broker_info{topics = Topics}] ->
      F =
        fun(TopicUri) ->
          ok = int_unsubscribe(TopicUri, SessionId, State),
          false
        end,
      lists:filter(F, Topics),
      ok;
    [] ->
      ok
  end.

%% @private
create_topic(Uri, Match, Sessions, #state{ets = Ets} = State) ->
  ID = erwa_support:gen_id(),
  Created = erlang:universaltime(),
  Topic = #topic{uri = Uri, id = ID, match = Match, created = Created, subscribers = Sessions},
  case ets:insert_new(Ets, [#id_topic{id = ID, topic = Uri}, Topic]) of
    true ->
      {ok, ID, #{id => ID,
        created => cowboy_clock:rfc1123(Created),
        uri => Uri,
        match => Match}};
    false ->
      create_topic(Uri, Match, Sessions, State)
  end.

%% @private
add_topic_to_session(Topic, SessionId, #state{ets = Ets}) ->
  case ets:lookup(Ets, {sess, SessionId}) of
    [#id_broker_info{topics = Topics} = IdInf] ->
      true = ets:insert(Ets, IdInf#id_broker_info{topics = [Topic | lists:delete(Topic, Topics)]}),
      ok;
    [] ->
      IdInf = #id_broker_info{id = {sess, SessionId}, topics = [Topic]},
      true = ets:insert_new(Ets, IdInf),
      ok
  end.

%% @private
remove_topic_from_session(Topic, SessionId, #state{ets = Ets}) ->
  [#id_broker_info{topics = Topics} = IdInf] = ets:lookup(Ets, {sess, SessionId}),
  case lists:delete(Topic, Topics) of
    [] ->
      true = ets:delete(Ets, {sess, SessionId});
    NewTopics ->
      true = ets:insert(Ets, IdInf#id_broker_info{topics = NewTopics})
  end,
  ok.

%% @private
publish_metaevent(_, _, _, _, #state{meta_events = disabled}) -> ok;
publish_metaevent(Event, TopicUri, SessionId, SecondArg, #state{ets = Ets}) ->
  case binary:part(TopicUri, {0, 5}) == <<"wamp.">> of
    true -> % do not fire metaevents on wamp. uris
      ok;
    false ->
      MetaTopic = case Event of
                    on_create -> <<"wamp.subscription.on_create">>;
                    on_subscribe -> <<"wamp.subscription.on_subscribe">>;
                    on_unsubscribe -> <<"wamp.subscription.on_unsubscribe">>;
                    on_delete -> <<"wamp.subscription.on_delete">>
                  end,
      {ok, _} = publish(MetaTopic, #{}, [SessionId, SecondArg], undefined, no_session, #data{ets = Ets})
  end,
  ok.