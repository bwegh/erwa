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
%% @doc the broker is a per realm gen_server that 

-module(erwa_broker).
-behaviour(gen_server).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



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

-record(id_info, {
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


-spec enable_metaevents(record(data) ) -> ok.
enable_metaevents( #data{pid=Pid} ) ->
	gen_server:call(Pid,enable_metaevents).

-spec disable_metaevents( record(data) ) -> ok.
disable_metaevents( #data{pid=Pid} ) ->
	gen_server:call(Pid,disable_metaevents).

-spec get_subscriptions( record(data) ) -> map().
get_subscriptions(#data{pid=Pid}) ->
	gen_server:call(Pid,get_subscriptions).

-spec get_subscription( record(data), non_neg_integer() ) -> map().
get_subscription(#data{pid=Pid},SubscriptionId) ->
	gen_server:call(Pid,{get_subscription,SubscriptionId}).



-spec subscribe(Topic::binary(),Options::map(), SessionId :: non_neg_integer(), Data::record(data)) -> {ok, non_neg_integer()}.
subscribe(Topic,Options,SessionId,#data{pid=Pid}) ->
	gen_server:call(Pid, {subscribe,Topic,Options,SessionId} ).

-spec unsubscribe(SubscriptionId::non_neg_integer(), SessionId::non_neg_integer(), Data::record(data)) -> ok | {error, Reason::term()}.
unsubscribe(SubscriptionId,SessionId,#data{pid=Pid}) ->
	gen_server:call(Pid, {unsubscribe,SubscriptionId,SessionId} ).

-spec unsubscribe_all(SessionId::non_neg_integer(), Data::record(data)) -> ok.
unsubscribe_all(SessionId, #data{pid=Pid}) ->
	gen_server:call(Pid,{unsubscribe_all,SessionId}).

-spec publish(Topic::binary(),Options::map(),Arguments :: list(), ArgumentsKw :: map(), Session :: term(), Data::record(data)) ->
	{ok, non_neg_integer()}.

publish(TopicUri,Options,Arguments,ArgumentsKw,SessionId,#data{ets=Ets}) ->
	case ets:lookup(Ets,TopicUri) of
		[#topic{subscribers=Subs,id=SubscriptionId}] ->
			{ok,PublicationID} = erwa_publications:get_pub_id(),
			Receipients = case maps:get(exclude_me,Options,true) of
											false ->
												Subs;
											_ ->
												lists:delete(SessionId,Subs)
										end,
			Details = case maps:get(disclose_me,Options,false) of
									true -> #{publisher => SessionId};
									_ -> #{}
								end,

			ToExclude = maps:get(exclude,Options,[]),
			ToEligible = maps:get(eligible,Options,Receipients),
			SendFilter = fun(SessId) ->
											 case (not lists:member(SessId,ToExclude)) and lists:member(SessId,ToEligible) of
												 true ->
													 Msg = {event,SubscriptionId,PublicationID,Details,Arguments,ArgumentsKw},
													 erwa_sessions:send_message_to(Msg,SessId),
													 true;
												 false ->
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


handle_call({subscribe,TopicUri,Options,SessionId}, _From, State) ->
	Result = int_subscribe(TopicUri,Options,SessionId,State),
	{reply,Result,State};
handle_call({unsubscribe,SubscriptionId,SessionId}, _From, State) ->
	Result = int_unsubscribe(SubscriptionId,SessionId,State),
	{reply,Result,State};
handle_call({unsubscribe_all, SessionId}, _From, #state{ets=_Ets} = State) ->
	Result = unsubscribe_all_for(SessionId,State),
	{reply,Result,State};
handle_call(get_data, _From, #state{ets=Ets} = State) ->
	{reply,{ok,#data{ets=Ets, pid=self()}},State};
handle_call(get_subscriptions, _From, #state{ets=Ets} = State) ->
	Exact = lists:flatten(ets:match(Ets,#topic{match = exact, id='$1', _='_'})),
	Prefix = lists:flatten(ets:match(Ets,#topic{match = prefix, id='$1', _='_'})),
	Wildcard = lists:flatten(ets:match(Ets,#topic{match = wildcard, id='$1', _='_'})),
	{reply,{ok,#{exact => Exact, prefix => Prefix, wildcard => Wildcard}},State};
handle_call({get_subscription,SubscriptionId}, _From, #state{ets=Ets} = State) ->
	case ets:lookup(Ets,SubscriptionId) of
		[#id_topic{id=SubscriptionId,topic=Uri}] ->
			[#topic{uri=Uri,match=Match,created=Created,id=Id}] = ets:lookup(Ets,Uri),
			{reply,{ok,#{uri => Uri, id => Id, match => Match, created => Created}},State};
		[] ->
			{reply,{error,not_found},State}
	end;
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



-spec int_subscribe(TopicUri :: binary(), Options :: map(), SessionId::non_neg_integer(), State :: record(state)) ->
	{ok, ID::non_neg_integer()} | {error, Reason :: term()}.
int_subscribe(TopicUri,Options,SessionId,#state{ets=Ets}=State) ->
	Match = maps:get(match,Options,exact),
	case Match of
		prefix ->
			{error,not_supported};
		wildcard ->
			{error,not_supported};
		exact ->
			{SubscriptionId,Created,TopicDetails} = case ets:lookup(Ets,TopicUri) of
																								[#topic{id=SID,subscribers=Subs}=T] ->
																									NewSubs = [SessionId|lists:delete(SessionId,Subs)],
																									ets:insert(Ets,T#topic{subscribers=NewSubs}),
																									{SID,false,not_needed};
																								[] ->
																									{ok,SID,TDetails} = create_topic(TopicUri,Match,[SessionId],State),
																									{SID,true,TDetails}
																							end,
			ok = add_topic_to_session(TopicUri,SessionId,State),
			case Created of
				true ->
					publish_metaevent(on_create,TopicUri,SessionId,TopicDetails,State);
				false -> nothing
			end,
			publish_metaevent(on_subscribe,TopicUri,SessionId,SubscriptionId,State),
			{ok,SubscriptionId}
	end.

-spec int_unsubscribe(IdOrTopic :: non_neg_integer() | binary(), SessionId :: non_neg_integer(), State :: record(state)) ->
	ok | {error, Reason :: term()}.

int_unsubscribe(SubscriptionId,SessionId,#state{ets=Ets}=State) when is_integer(SubscriptionId) ->
	case ets:lookup(Ets,SubscriptionId) of
		[#id_topic{id=SubscriptionId,topic=TopicUri}] ->
			int_unsubscribe(TopicUri,SessionId,State);
		[] ->
			{error,not_found}
	end;
int_unsubscribe(TopicUri,SessionId,#state{ets=Ets}=State) when is_binary(TopicUri) ->
	[#topic{subscribers=Subs,id=SubscriptionId,uri=TopicUri}=T] = ets:lookup(Ets,TopicUri),
	case {lists:member(SessionId,Subs),lists:delete(SessionId,Subs)} of
		{false,_} ->
			{error, not_subscribed};
		{true,[]} ->
			ok = remove_topic_from_session(TopicUri,SessionId,State),
			true = ets:delete(Ets,SubscriptionId),
			true = ets:delete(Ets,TopicUri),
			publish_metaevent(on_unsubscribe,TopicUri,SessionId,SubscriptionId,State),
			publish_metaevent(on_delete,TopicUri,SessionId,SubscriptionId,State),
			ok;
		{true,NewSubs} ->
			ok = remove_topic_from_session(TopicUri,SessionId,State),
			true = ets:insert(Ets,T#topic{subscribers=NewSubs}),
			publish_metaevent(on_unsubscribe,TopicUri,SessionId,SubscriptionId,State),
			ok
	end.


-spec unsubscribe_all_for( SessionId :: non_neg_integer(), State :: record(state)) ->
	ok | {error, Reason :: term()}.
unsubscribe_all_for(SessionId,#state{ets=Ets}=State) ->
	case ets:lookup(Ets,{sess,SessionId}) of
		[#id_info{topics=Topics}] ->
			F = fun(TopicUri) ->
							ok = int_unsubscribe(TopicUri,SessionId,State),
							false
					end,
			lists:filter(F,Topics),
			ok;
		[] ->
			ok
	end.

create_topic(Uri,Match,Sessions,#state{ets=Ets}=State) ->
	ID = gen_id(),
	Created = erlang:universaltime(),
	Topic = #topic{uri=Uri,id=ID,match=Match,created=Created,subscribers=Sessions},
	case ets:insert_new(Ets,[#id_topic{id=ID,topic=Uri},Topic]) of
		true ->
			{ok,ID,#{id => ID,
							 created => cowboy_clock:rfc1123(Created),
							 uri => Uri,
							 match => Match}};
		false ->
			create_topic(Uri,Match,Sessions,State)
	end.

add_topic_to_session(Topic,SessionId,#state{ets=Ets}) ->
	case ets:lookup(Ets,{sess,SessionId}) of
		[#id_info{topics=Topics} = IdInf] ->
			true = ets:insert(Ets,IdInf#id_info{topics=[Topic|lists:delete(Topic,Topics)]}),
			ok;
		[] ->
			IdInf = #id_info{id={sess,SessionId},topics=[Topic]},
			true = ets:insert_new(Ets,IdInf),
			ok
	end.


remove_topic_from_session(Topic,SessionId,#state{ets=Ets}) ->
	[#id_info{topics=Topics} = IdInf] = ets:lookup(Ets,{sess,SessionId}),
	case lists:delete(Topic,Topics) of
		[] ->
			true = ets:delete(Ets,{sess,SessionId});
		NewTopics ->
			true = ets:insert(Ets,IdInf#id_info{topics=NewTopics})
	end,
	ok.


gen_id() ->
	crypto:rand_uniform(0,9007199254740992).

publish_metaevent(_,_,_,_,#state{meta_events=disabled}) ->
	ok;
publish_metaevent(Event,TopicUri,SessionId,SecondArg,#state{ets=Ets}) ->
	case binary:part(TopicUri,{0,5}) == <<"wamp.">> of
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
	ok = enable_metaevents(Data),
	{ok,stopped} = stop(Data).

un_subscribe_test() ->
	{ok,Pid} = start(),
	{ok,Data} = get_data(Pid),
	ok = disable_metaevents(Data),
	SessionId = gen_id(),
	0 = get_tablesize(Data),
	{ok,ID1} = subscribe(<<"topic.test1">>,#{},SessionId,Data),
	3 = get_tablesize(Data),
	{ok,ID2} = subscribe(<<"topic.test2">>,#{},SessionId,Data),
	5 = get_tablesize(Data),
	ok = unsubscribe(ID1,SessionId,Data),
	3 = get_tablesize(Data),
	{error,not_found} = unsubscribe(ID1,SessionId,Data),
	ok = unsubscribe(ID2,SessionId,Data),
	0 = get_tablesize(Data),
	{error,not_found} = unsubscribe(ID2,SessionId,Data),
	0 = get_tablesize(Data),
	{ok,stopped} = stop(Data).

unsubscribe_all_test() ->
	{ok,Pid} = start(),
	{ok,Data} = get_data(Pid),
	ok = disable_metaevents(Data),
	SessionId = gen_id(),
	0 = get_tablesize(Data),
	ok = unsubscribe_all(SessionId,Data),
	0 = get_tablesize(Data),
	{ok,ID1} = subscribe(<<"topic.test1">>,#{},SessionId,Data),
	3 = get_tablesize(Data),
	{ok,ID2} = subscribe(<<"topic.test2">>,#{},SessionId,Data),
	5 = get_tablesize(Data),
	ok = unsubscribe_all(SessionId,Data),
	0 = get_tablesize(Data),
	{error,not_found} = unsubscribe(ID1,SessionId,Data),
	0 = get_tablesize(Data),
	{error,not_found} = unsubscribe(ID2,SessionId,Data),
	0 = get_tablesize(Data),
	ok = unsubscribe_all(SessionId,Data),
	0 = get_tablesize(Data),
	{ok,stopped} = stop(Data).


multiple_un_subscribe_test() ->
	erwa_sessions:create_table(),
	{ok,Pid} = start(),
	{ok,Data} = get_data(Pid),
	ok = disable_metaevents(Data),
	{ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
	0 = get_tablesize(Data),
	{ok,ID1} = subscribe(<<"topic.test1">>,#{},SessionId,Data),
	3 = get_tablesize(Data),
	{ok,ID2} = subscribe(<<"topic.test2">>,#{},SessionId,Data),
	5 = get_tablesize(Data),
	MyPid = self(),
	F = fun() ->
					{ok,S2} = erwa_sessions:register_session(<<"erwa.test">>),
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
					ok = erwa_broker:unsubscribe_all(S2,Data),
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
	ok = unsubscribe(ID1,SessionId,Data),
	3 = get_tablesize(Data),
	ok = unsubscribe_all(SessionId,Data),
	0 = get_tablesize(Data),
	erwa_sessions:drop_table(),
	{ok,stopped} = stop(Data).


publish_test() ->
	erwa_sessions:create_table(),
    ok = erwa_publications:create_table(),
	{ok,Pid} = start(),
	{ok,Data} = get_data(Pid),
	ok = disable_metaevents(Data),
	{ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
	{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId,Data),
	MyPid = self(),
	F = fun() ->
					{ok,S2} = erwa_sessions:register_session(<<"erwa.test">>),
					{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},S2,Data),
					MyPid ! subscribed,
					receive
						{erwa,{event,ID,PubId,#{},undefined,undefined}} ->
							MyPid ! {received,PubId}
					end,
					ok = erwa_broker:unsubscribe_all(S2,Data),
					ok
			end,
	spawn(F),
	receive
		subscribed -> ok
	end,
	{ok,PublicationID1} = publish(<<"topic.test1">>,#{},undefined,undefined,SessionId,Data),
	receive
		{received,PublicationID1} -> ok
	end,
	{ok,PublicationID2} = publish(<<"topic.test1">>,#{exclude_me=>false},undefined,undefined,SessionId,Data),
	ok = receive
				 {erwa,{event,ID,PublicationID2,#{},undefined,undefined}} ->
					 ok
			 end,
	erwa_sessions:drop_table(),
	{ok,stopped} = stop(Data),
    ok = erwa_publications:drop_table().


exclude_test() ->
	erwa_sessions:create_table(),
    ok = erwa_publications:create_table(),
	{ok,Pid} = start(),
	{ok,Data} = get_data(Pid),
	ok = disable_metaevents(Data),
	{ok,SessionId1} = erwa_sessions:register_session(<<"erwa.test">>),
	{ok,SessionId2} = erwa_sessions:register_session(<<"erwa.test">>),
	{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId1,Data),
	MyPid = self(),
	F = fun() ->
					{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId2,Data),
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
					ok = erwa_broker:unsubscribe_all(SessionId2,Data),
					MyPid ! done,
					ok
			end,
	ClientPid = spawn(F),
	receive
		subscribed -> ok
	end,
	{ok,PubID} = publish(<<"topic.test1">>,#{exclude_me => false,exclude => [SessionId2]},undefined,undefined,SessionId1,Data),
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
	erwa_sessions:drop_table(),
	{ok,stopped} = stop(Data),
    ok = erwa_publications:drop_table().



eligible_test() ->
	erwa_sessions:create_table(),
    ok = erwa_publications:create_table(),
	{ok,Pid} = start(),
	{ok,Data} = get_data(Pid),
	ok = disable_metaevents(Data),
	{ok, SessionId1} = erwa_sessions:register_session(<<"erwa.test">>),
	{ok, SessionId2} = erwa_sessions:register_session(<<"erwa.test">>),

	{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId1,Data),
	MyPid = self(),
	F = fun() ->
					{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId2,Data),
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
					ok = erwa_broker:unsubscribe_all(SessionId2,Data),
					MyPid ! done,
					ok
			end,
	ClientPid = spawn(F),
	receive
		subscribed -> ok
	end,
	{ok,PubID} = publish(<<"topic.test1">>,#{exclude_me=>false,eligible=>[SessionId1]},undefined,undefined,SessionId1,Data),
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
	erwa_sessions:drop_table(),
	{ok,stopped} = stop(Data),
    ok = erwa_publications:drop_table().


garbage_test() ->
	{ok,Pid} = start(),
	ignored = gen_server:call(Pid,some_garbage),
	ok = gen_server:cast(Pid,some_garbage),
	Pid ! some_garbage,
	{ok,stopped} = stop(Pid).


-endif.
