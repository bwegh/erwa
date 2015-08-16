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
%% @doc the broker  

-module(erwa_broker).


-export([create_table/0]).
-export([drop_tables/0]).

%% API
-export([subscribe/4]).
-export([unsubscribe/2]).
-export([unsubscribe_all/1]).
-export([publish/6]).

-export([get_subscriptions/1]).
-export([get_subscription_details/1]).


-record(erwa_subscription, {
		  id = unknown,
		  uri = unknown,
		  realm = unknown,
		  match = exact,
		  created = unknown,
		  subscribers = []
		 }).



-spec create_table() -> ok.
create_table() ->
	{atomic, ok} = mnesia:create_table(erwa_subscription, [{disc_copies, []},
														   {ram_copies,
															[node()]}, {type,
																		set},
														   {attributes,
															record_info(fields,
																		erwa_subscription)},{index,[uri,match,realm]}]),
	ok.

-spec drop_tables() -> ok.
drop_tables() ->
	{atomic, ok} = mnesia:delete_table(erwa_subscription),
	ok.




-spec subscribe(Topic::binary(),Options::map(), SessionId :: non_neg_integer(), Realm :: binary())-> {ok, non_neg_integer()}.
subscribe(Topic,Options,SessionId,Realm) ->
	Match = maps:get(match,Options,exact),
	case Match of 
		prefix -> 
			{error,not_supported};
		wildcard -> 
			{error, not_supported};
		exact -> 
			% just exact for now, others will follow 
			Id = gen_id(),
			InsertSub = fun() -> 
								case mnesia:read(erwa_subscription, Id) of
									[] -> 
										Subscriptions =
										mnesia:index_read(erwa_subscription,
														  Topic, uri),
										Filter = fun(#erwa_subscription{realm=R}=Sub, All) ->
														 case R of
															 Realm ->
																 [Sub|All];
															 _ -> 
																 All
														 end 
												 end,
										case lists:foldl(Filter,[],Subscriptions) of 
											[] -> 
												ok =
												mnesia:write(#erwa_subscription{uri=Topic,
																				match=exact,
																				realm=Realm,
																				id=Id,
																				created=calendar:universal_time(),
																				subscribers=[SessionId]}),
												ok = erwa_sessions:add_subscription(Id,SessionId),
												Id;
											[#erwa_subscription{subscribers=Subs, id=SubId}=Subscription] ->
												NewSubs = [SessionId | lists:delete(SessionId,Subs)],
												ok = mnesia:write(Subscription#erwa_subscription{subscribers=NewSubs}),
												ok = erwa_sessions:add_subscription(SubId,SessionId),
												SubId
										end;
									[#erwa_subscription{}] -> mnesia:abort(already_exist)
								end 
						end, 
			case mnesia:transaction(InsertSub) of
				{atomic, SubId} ->
					{ok, SubId};
				{aborted, already_exist} ->
					subscribe(Topic, Options, SessionId, Realm)
			end
	end.




-spec unsubscribe(SubscriptionId::non_neg_integer(), SessionId::non_neg_integer()) -> ok | {error, Reason::term()}.
unsubscribe(SubscriptionId,SessionId) ->
	Remove = fun() -> 
					 case mnesia:read(erwa_subscription, SubscriptionId) of
						 [] -> mnesia:abort(not_found);
						 [#erwa_subscription{subscribers=Subs, id=SubscriptionId}=Subscription] -> 
							 NewSubs = lists:delete(SessionId,Subs),
							 ok =
							 mnesia:write(Subscription#erwa_subscription{subscribers=NewSubs}),
							 ok =
							 erwa_sessions:rem_subscription(SubscriptionId,
															SessionId),
							 ok
					 end 
			 end,
	{atomic,Res} = mnesia:transaction(Remove),
	Res.

-spec unsubscribe_all(SessionId::non_neg_integer()) -> ok.
unsubscribe_all(SessionId) ->
	Subs = erwa_sessions:get_subscriptions(SessionId),
	unsubscribe_all(Subs,SessionId).

unsubscribe_all([],_SessionId) -> 
	ok;
unsubscribe_all([H|T],SessionId) ->
	ok = unsubscribe(H,SessionId),
	unsubscribe_all(T,SessionId).


-spec publish(Topic::binary(),Options::map(),Arguments :: list(), ArgumentsKw :: map(), Session :: term(), Realm :: binary()) ->
	{ok, non_neg_integer()}.

publish(TopicUri,Options,Arguments,ArgumentsKw,SessionId,Realm) ->
	GetIds = fun() -> 
					 % just exact for now ..
					 case mnesia:index_read(erwa_subscription, TopicUri, uri) of
						 [] -> [];
						 Subscriptions ->
							 GetSubs = fun(#erwa_subscription{id=SubId, subscribers=S, realm=R},Subs) ->
											  case R of 
												  Realm -> [{SubId, S}|Subs];
												  _ -> Subs 
											  end 
									  end,
							 lists:foldl(GetSubs, [], Subscriptions)
					 end 
			 end,
	{atomic, [{SubscriptionId,Subs}]} = mnesia:transaction(GetIds),
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
	{ok,PublicationID}.


get_subscriptions(Realm)  ->
	GetSubs = fun() -> 
					  mnesia:index_read(erwa_subscription, Realm ,realm)
			  end,
	{atomic, Subscriptions} = mnesia:transaction(GetSubs),
	Sort = fun(#erwa_subscription{id=Id, match=Match}, {Exact, Prefix,
														Wildcard}) ->
				   case Match of
					  exact -> 
						   {[Id|Exact], Prefix, Wildcard};
					  prefix ->
						   {Exact, [Id | Prefix], Wildcard};
					  wildcard ->
						   {Exact, Prefix, [Id | Wildcard]}
				   end 
		   end,
   	{Exact, Prefix, Wildcard} = lists:foldl(Sort, {[],[],[]}, Subscriptions),
	{ok,#{exact => Exact, prefix => Prefix, wildcard => Wildcard}}.


get_subscription_details(SubscriptionId)  ->
	GetSubscription = fun() ->
							  mnesia:read(erwa_subscription, SubscriptionId)
					  end,
	{atomic, #erwa_subscription{uri=Uri, match=Match, created=Created, id=Id}} = mnesia:transaction(GetSubscription),
	{ok,#{uri => Uri, id => Id, match => Match, created => Created}}.



gen_id() ->
	crypto:rand_uniform(0,9007199254740992).



%% -ifdef(TEST).
%%
%% get_tablesize(#data{ets=Ets}) ->
%% 	ets:info(Ets,size).
%%
%% start_stop_test() ->
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	0 = get_tablesize(Data),
%% 	ok = enable_metaevents(Data),
%% 	{ok,stopped} = stop(Data).
%%
%% un_subscribe_test() ->
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	SessionId = gen_id(),
%% 	0 = get_tablesize(Data),
%% 	{ok,ID1} = subscribe(<<"topic.test1">>,#{},SessionId,Data),
%% 	3 = get_tablesize(Data),
%% 	{ok,ID2} = subscribe(<<"topic.test2">>,#{},SessionId,Data),
%% 	5 = get_tablesize(Data),
%% 	ok = unsubscribe(ID1,SessionId,Data),
%% 	3 = get_tablesize(Data),
%% 	{error,not_found} = unsubscribe(ID1,SessionId,Data),
%% 	ok = unsubscribe(ID2,SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	{error,not_found} = unsubscribe(ID2,SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	{ok,stopped} = stop(Data).
%%
%% unsubscribe_all_test() ->
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	SessionId = gen_id(),
%% 	0 = get_tablesize(Data),
%% 	ok = unsubscribe_all(SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	{ok,ID1} = subscribe(<<"topic.test1">>,#{},SessionId,Data),
%% 	3 = get_tablesize(Data),
%% 	{ok,ID2} = subscribe(<<"topic.test2">>,#{},SessionId,Data),
%% 	5 = get_tablesize(Data),
%% 	ok = unsubscribe_all(SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	{error,not_found} = unsubscribe(ID1,SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	{error,not_found} = unsubscribe(ID2,SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	ok = unsubscribe_all(SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	{ok,stopped} = stop(Data).
%%
%%
%% multiple_un_subscribe_test() ->
%% 	erwa_sessions:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
%% 	0 = get_tablesize(Data),
%% 	{ok,ID1} = subscribe(<<"topic.test1">>,#{},SessionId,Data),
%% 	3 = get_tablesize(Data),
%% 	{ok,ID2} = subscribe(<<"topic.test2">>,#{},SessionId,Data),
%% 	5 = get_tablesize(Data),
%% 	MyPid = self(),
%% 	F = fun() ->
%% 					{ok,S2} = erwa_sessions:register_session(<<"erwa.test">>),
%% 					{ok,ID3} = erwa_broker:subscribe(<<"topic.test1">>,#{},S2,Data),
%% 					MyPid ! {first_subscription,ID3},
%% 					receive
%% 					after 200 -> ok
%% 					end,
%% 					{ok,ID4} = erwa_broker:subscribe(<<"topic.test2">>,#{},S2,Data),
%% 					MyPid ! {second_subscription,ID4},
%% 					receive
%% 					after 200 -> ok
%% 					end,
%% 					ok = erwa_broker:unsubscribe_all(S2,Data),
%% 					MyPid ! done,
%% 					ok
%% 			end,
%% 	spawn(F),
%% 	receive
%% 		{first_subscription,ID1} ->
%% 			ok
%% 	end,
%% 	6 = get_tablesize(Data),
%% 	receive
%% 		{second_subscription,ID2} ->
%% 			ok
%% 	end,
%% 	6 = get_tablesize(Data),
%% 	receive
%% 		done ->
%% 			ok
%% 	end,
%% 	5 = get_tablesize(Data),
%% 	ok = unsubscribe(ID1,SessionId,Data),
%% 	3 = get_tablesize(Data),
%% 	ok = unsubscribe_all(SessionId,Data),
%% 	0 = get_tablesize(Data),
%% 	erwa_sessions:drop_table(),
%% 	{ok,stopped} = stop(Data).
%%
%%
%% publish_test() ->
%% 	erwa_sessions:create_table(),
%%     ok = erwa_publications:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
%% 	{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId,Data),
%% 	MyPid = self(),
%% 	F = fun() ->
%% 					{ok,S2} = erwa_sessions:register_session(<<"erwa.test">>),
%% 					{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},S2,Data),
%% 					MyPid ! subscribed,
%% 					receive
%% 						{erwa,{event,ID,PubId,#{},undefined,undefined}} ->
%% 							MyPid ! {received,PubId}
%% 					end,
%% 					ok = erwa_broker:unsubscribe_all(S2,Data),
%% 					ok
%% 			end,
%% 	spawn(F),
%% 	receive
%% 		subscribed -> ok
%% 	end,
%% 	{ok,PublicationID1} = publish(<<"topic.test1">>,#{},undefined,undefined,SessionId,Data),
%% 	receive
%% 		{received,PublicationID1} -> ok
%% 	end,
%% 	{ok,PublicationID2} = publish(<<"topic.test1">>,#{exclude_me=>false},undefined,undefined,SessionId,Data),
%% 	ok = receive
%% 				 {erwa,{event,ID,PublicationID2,#{},undefined,undefined}} ->
%% 					 ok
%% 			 end,
%% 	erwa_sessions:drop_table(),
%% 	{ok,stopped} = stop(Data),
%%     ok = erwa_publications:drop_table().
%%
%%
%% exclude_test() ->
%% 	erwa_sessions:create_table(),
%%     ok = erwa_publications:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok,SessionId1} = erwa_sessions:register_session(<<"erwa.test">>),
%% 	{ok,SessionId2} = erwa_sessions:register_session(<<"erwa.test">>),
%% 	{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId1,Data),
%% 	MyPid = self(),
%% 	F = fun() ->
%% 					{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId2,Data),
%% 					MyPid ! subscribed,
%% 					Received  = receive
%% 												{erwa,{event,ID,_,#{},undefined,undefined}} ->
%% 													true;
%% 												got_something ->
%% 													MyPid ! nothing,
%% 													false
%% 											end,
%% 					case Received of
%% 						true ->
%% 							receive
%% 								{got_something} ->
%% 									MyPid ! yes_got_it
%% 							end;
%% 						false ->
%% 							ok
%% 					end,
%% 					ok = erwa_broker:unsubscribe_all(SessionId2,Data),
%% 					MyPid ! done,
%% 					ok
%% 			end,
%% 	ClientPid = spawn(F),
%% 	receive
%% 		subscribed -> ok
%% 	end,
%% 	{ok,PubID} = publish(<<"topic.test1">>,#{exclude_me => false,exclude => [SessionId2]},undefined,undefined,SessionId1,Data),
%% 	ok = receive
%% 				 {erwa,{event,ID,PubID,#{},undefined,undefined}} ->
%% 					 ok
%% 			 end,
%% 	receive
%% 	after 100 -> ok
%% 	end,
%% 	ClientPid ! got_something,
%% 	ok = receive
%% 				 nothing -> ok;
%% 				 yes_got_it -> wrong
%% 			 end,
%% 	erwa_sessions:drop_table(),
%% 	{ok,stopped} = stop(Data),
%%     ok = erwa_publications:drop_table().
%%
%%
%%
%% eligible_test() ->
%% 	erwa_sessions:create_table(),
%%     ok = erwa_publications:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok, SessionId1} = erwa_sessions:register_session(<<"erwa.test">>),
%% 	{ok, SessionId2} = erwa_sessions:register_session(<<"erwa.test">>),
%%
%% 	{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId1,Data),
%% 	MyPid = self(),
%% 	F = fun() ->
%% 					{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId2,Data),
%% 					MyPid ! subscribed,
%% 					Received  = receive
%% 												{erwa,{event,ID,_,[],undefined,undefined}} ->
%% 													true;
%% 												got_something ->
%% 													MyPid ! nothing,
%% 													false
%% 											end,
%% 					case Received of
%% 						true ->
%% 							receive
%% 								{got_something} ->
%% 									MyPid ! yes_got_it
%% 							end;
%% 						false ->
%% 							ok
%% 					end,
%% 					ok = erwa_broker:unsubscribe_all(SessionId2,Data),
%% 					MyPid ! done,
%% 					ok
%% 			end,
%% 	ClientPid = spawn(F),
%% 	receive
%% 		subscribed -> ok
%% 	end,
%% 	{ok,PubID} = publish(<<"topic.test1">>,#{exclude_me=>false,eligible=>[SessionId1]},undefined,undefined,SessionId1,Data),
%% 	ok = receive
%% 				 {erwa,{event,ID,PubID,#{},undefined,undefined}} ->
%% 					 ok
%% 			 end,
%% 	receive
%% 	after 100 -> ok
%% 	end,
%% 	ClientPid ! got_something,
%% 	ok = receive
%% 				 nothing -> ok;
%% 				 yes_got_it -> wrong
%% 			 end,
%% 	erwa_sessions:drop_table(),
%% 	{ok,stopped} = stop(Data),
%%     ok = erwa_publications:drop_table().
%%
%%
%% garbage_test() ->
%% 	{ok,Pid} = start(),
%% 	ignored = gen_server:call(Pid,some_garbage),
%% 	ok = gen_server:cast(Pid,some_garbage),
%% 	Pid ! some_garbage,
%% 	{ok,stopped} = stop(Pid).
%%
%%
%% -endif.
