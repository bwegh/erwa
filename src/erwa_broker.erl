%%
%%lol
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


-export([init/1]).
-export([cleanup/1]).

%% API
-export([subscribe/4]).
-export([unsubscribe/3]).
-export([unsubscribe_all/2]).
-export([publish/6]).

-export([get_subscriptions/1]).
-export([get_subscription_details/2]).


-record(erwa_subscription, {
		  id = unknown,
		  uri = unknown,
		  realm = unknown,
		  match = exact,
		  created = unknown,
		  subscribers = []
		 }).



-spec init(Realm :: binary()) -> ok.
init(Realm) ->
	create_table_for_realm(Realm).

-spec cleanup(Realm :: binary()) -> ok.
cleanup(Realm) ->
	delete_table_for_realm(Realm).



-spec subscribe(Topic::binary(),Options::map(), SessionId :: non_neg_integer(), Realm :: binary())-> {ok, non_neg_integer()}.
subscribe(Topic,Options,SessionId,Realm) ->
	Match = get_match_type(Options),
	case Match of 
		prefix -> 
			{error,not_supported};
		wildcard -> 
			{error, not_supported};
		exact -> 
			% just exact for now, others will follow 
			Id = gen_id(),
			Table = realm_to_db_name(Realm),
			InsertSub = fun() -> 
								case mnesia:read(Table, Id) of
									[] -> 
										Subscriptions =
										mnesia:index_read(Table,
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
                                                Created = calendar:universal_time(),
												ok =
												mnesia:write(Table,
															 #erwa_subscription{uri=Topic,
																				match=exact,
																				realm=Realm,
																				id=Id,
																				created=Created,
																				subscribers=[SessionId]},
															write),
												ok = erwa_sess_man:add_subscription(Id,SessionId),
                                                Details = #{ id=> Id,
                                                             created => Created,
                                                             uri => Topic, 
                                                             match => exact },
                                                publish_metaevent(on_create,Topic,SessionId, Details,Realm),
                                                publish_metaevent(on_subscribe, Topic, SessionId, Id , Realm),
                                                {ok,Id};
											[#erwa_subscription{subscribers=Subs, id=SubId}=Subscription] ->
												NewSubs = [SessionId | lists:delete(SessionId,Subs)],
												ok =
												mnesia:write(Table,Subscription#erwa_subscription{subscribers=NewSubs},write),
												ok = erwa_sess_man:add_subscription(SubId,SessionId),
                                                publish_metaevent(on_subscribe, Topic, SessionId, SubId , Realm),
                                                {ok,SubId}
                                        end;
                                    [#erwa_subscription{}] -> {error, already_exist}
								end 
						end, 
            {atomic, Result} = mnesia:transaction(InsertSub),
            case Result of 
                {error, already_exist} -> 
                    subscribe(Topic, Options, SessionId, Realm);
                {ok, SubId} ->
                    {ok, SubId}
            end
	end.



get_match_type(Options) ->
	maps:get(match,Options,exact).


-spec unsubscribe(SubscriptionId::non_neg_integer(),
				  SessionId::non_neg_integer(), Realm::binary()) -> ok | {error, Reason::term()}.
unsubscribe(SubscriptionId,SessionId, Realm) ->
	Database = realm_to_db_name(Realm),
	Remove = fun() -> 
					 case mnesia:read(Database, SubscriptionId) of
						 [] -> mnesia:abort(not_found);
                         [#erwa_subscription{subscribers=Subs, id=SubscriptionId, uri=TopicUri}=Subscription] -> 
                             NewSubs = lists:delete(SessionId,Subs),
                             publish_metaevent(on_unsubscribe, TopicUri, SessionId, SubscriptionId, Realm),
                             case NewSubs of 
                                 [] -> 
                                     ok = mnesia:delete(Database, SubscriptionId, write),
                                     publish_metaevent(on_delete, TopicUri, SessionId, SubscriptionId, Realm);
                                 NewSubs -> 
                                     ok = mnesia:write(Database,Subscription#erwa_subscription{subscribers=NewSubs},write),
                                     ok = erwa_sess_man:rem_subscription(SubscriptionId, SessionId)
                             end,
                             ok
					 end 
			 end,
	{atomic,Res} = mnesia:transaction(Remove),
	Res.

-spec unsubscribe_all(SessionId::non_neg_integer(),Realm::binary()) -> ok.
unsubscribe_all(SessionId,Realm) ->
	Subs = erwa_sess_man:get_subscriptions(SessionId),
	unsubscribe_all(Subs,SessionId,Realm).

unsubscribe_all([],_SessionId,_Realm) -> 
	ok;
unsubscribe_all([H|T],SessionId,Realm) ->
	ok = unsubscribe(H,SessionId,Realm),
	unsubscribe_all(T,SessionId).


-spec publish(Topic::binary(),Options::map(),Arguments :: list(), ArgumentsKw :: map(), Session :: term(), Realm :: binary()) ->
	{ok, non_neg_integer()}.

publish(TopicUri,Options,Arguments,ArgumentsKw,SessionId,Realm) ->
	Database = realm_to_db_name(Realm),
	GetIds = fun() -> 
					 % just exact for now ..
					 case mnesia:index_read(Database, TopicUri, uri) of
						 [] -> {none,[]};
						 [#erwa_subscription{id=SubId,subscribers=Subs}] ->
							{SubId,Subs}
					 end 
			 end,
	{atomic, {SubscriptionId,Subs}} = mnesia:transaction(GetIds),
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
								 erwa_sess_man:send_message_to(Msg,SessId),
								 true;
							 false ->
								 false
						 end
				 end,
	lists:filter(SendFilter,Receipients),
	{ok,PublicationID}.


get_subscriptions(Realm)  ->
	Database = realm_to_db_name(Realm),
	GetSubs = fun() -> 
					  mnesia:index_read(Database, Realm ,realm)
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


get_subscription_details(SubscriptionId, Realm)  ->
	Database = realm_to_db_name(Realm),
	GetSubscription = fun() ->
							  mnesia:read(Database, SubscriptionId)
					  end,
	{atomic, #erwa_subscription{uri=Uri, match=Match, created=Created, id=Id}} = mnesia:transaction(GetSubscription),
	{ok,#{uri => Uri, id => Id, match => Match, created => Created}}.



create_table_for_realm(Realm) ->
	Table = realm_to_db_name(Realm),
	case lists:member(Table, mnesia:system_info(local_tables)) of
		true ->
			mnesia:delete_table(Table);
		_-> do_nthing
	end,
	{atomic, ok} = mnesia:create_table(Table, [{disc_copies, []},
														   {ram_copies,
															[node()]}, 
														   {type, set},
														   {record_name,
															erwa_subscription},
														   {attributes,
															record_info(fields,
																		erwa_subscription)},{index,[uri,match,realm]}]),
	ok.

delete_table_for_realm(Realm) ->
	{atomic, ok} = mnesia:delete_table(realm_to_db_name(Realm)),
	ok.


realm_to_db_name(Realm) ->
	Prefix = <<"erwa_subscription_">>,
	binary_to_atom(<< Prefix/binary, Realm/binary >>, utf8 ).

gen_id() ->
	crypto:rand_uniform(0,9007199254740993).

publish_metaevent(Event, TopicUri, SessionId, SecondArg, Realm) ->
    case binary:part(TopicUri,{0,5}) == <<"wamp.">> of
        true ->
            do_nothing;
        false ->
            MetaTopic = case Event of
                            on_create -> <<"wamp.subscription.on_create">>;
                            on_subscribe -> <<"wamp.subscription.on_subscribe">>;
                            on_unsubscribe -> <<"wamp.subscription.on_unsubscribe">>;
                            on_delete -> <<"wamp.subscription.on_delete">>
                        end,
            {ok, _} = publish(MetaTopic, #{}, [SessionId, SecondArg], undefined, no_session, Realm)     
    end,
    ok.

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
%% 	erwa_sess_man:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok,SessionId} = erwa_sess_man:register_session(<<"erwa.test">>),
%% 	0 = get_tablesize(Data),
%% 	{ok,ID1} = subscribe(<<"topic.test1">>,#{},SessionId,Data),
%% 	3 = get_tablesize(Data),
%% 	{ok,ID2} = subscribe(<<"topic.test2">>,#{},SessionId,Data),
%% 	5 = get_tablesize(Data),
%% 	MyPid = self(),
%% 	F = fun() ->
%% 					{ok,S2} = erwa_sess_man:register_session(<<"erwa.test">>),
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
%% 	erwa_sess_man:drop_table(),
%% 	{ok,stopped} = stop(Data).
%%
%%
%% publish_test() ->
%% 	erwa_sess_man:create_table(),
%%     ok = erwa_publications:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok,SessionId} = erwa_sess_man:register_session(<<"erwa.test">>),
%% 	{ok,ID} = erwa_broker:subscribe(<<"topic.test1">>,#{},SessionId,Data),
%% 	MyPid = self(),
%% 	F = fun() ->
%% 					{ok,S2} = erwa_sess_man:register_session(<<"erwa.test">>),
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
%% 	erwa_sess_man:drop_table(),
%% 	{ok,stopped} = stop(Data),
%%     ok = erwa_publications:drop_table().
%%
%%
%% exclude_test() ->
%% 	erwa_sess_man:create_table(),
%%     ok = erwa_publications:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok,SessionId1} = erwa_sess_man:register_session(<<"erwa.test">>),
%% 	{ok,SessionId2} = erwa_sess_man:register_session(<<"erwa.test">>),
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
%% 	erwa_sess_man:drop_table(),
%% 	{ok,stopped} = stop(Data),
%%     ok = erwa_publications:drop_table().
%%
%%
%%
%% eligible_test() ->
%% 	erwa_sess_man:create_table(),
%%     ok = erwa_publications:create_table(),
%% 	{ok,Pid} = start(),
%% 	{ok,Data} = get_data(Pid),
%% 	ok = disable_metaevents(Data),
%% 	{ok, SessionId1} = erwa_sess_man:register_session(<<"erwa.test">>),
%% 	{ok, SessionId2} = erwa_sess_man:register_session(<<"erwa.test">>),
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
%% 	erwa_sess_man:drop_table(),
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
