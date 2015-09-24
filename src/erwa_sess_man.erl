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

-module(erwa_sess_man).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([create_table/0]).
-export([drop_table/0]).

-export([register_session/1]).
-export([unregister_session/0]).
-export([get_realm/1]).

-export([add_subscription/2]).
-export([rem_subscription/2]).
-export([get_subscriptions/1]).
-export([clear_subscriptions/1]).

-export([add_registration/2]).
-export([rem_registration/2]).
-export([get_registrations/1]).
-export([clear_registrations/1]).

-export([get_session_count/1]).
-export([get_session_ids/1]).

-export([send_message_to/2]).


-record(erwa_session_record, {
					id = none,
					pid = unknown,
					realm = unknown,
					subscriptions = [],
					registrations = []
				 }).


-spec create_table() -> ok.
create_table() -> 
	case lists:member(erwa_session_record, mnesia:system_info(local_tables)) of
		true ->
			mnesia:delete_table(erwa_session_record);
		_-> do_nthing
	end,
	mnesia:create_table(erwa_session_record,[{disc_copies,[]}, {disc_only_copies, []},
																					 {ram_copies, [node()]}, {type, set},
																					 {attributes, record_info(fields,
																																		erwa_session_record)},
																					 {index, [pid, realm]}]),
	mnesia:wait_for_tables([erwa_session_record],60000),
	ok.


-spec drop_table() -> ok.
drop_table() ->
	mnesia:delete_table(erwa_session_record),
	ok.


-spec send_message_to(Msg :: term(), SessionId :: non_neg_integer()) -> ok | {error, unknown}.
send_message_to(Msg,SessionId) ->
    GetPid = fun() -> mnesia:read({erwa_session_record, SessionId}) end,
    case  mnesia:transaction(GetPid) of
        {atomic,[Session]} ->
            Session#erwa_session_record.pid ! {erwa,Msg};
        {atomic,[]} ->
            {error,unknown}
    end.


-spec register_session(Realm :: binary()) -> {ok,non_neg_integer()}.
register_session(Realm) ->
    case erwa_realms:exists(Realm) of
        true -> ID = add_session(self(), Realm),
                {ok, ID};
        false -> {error, no_such_realm}
    end.


-spec unregister_session() -> ok | not_found.
unregister_session() ->
	delete_session(self()).


-spec get_realm(SessionId :: non_neg_integer()) -> {ok, binary()} | {error, atom()}.
get_realm(SessionId) ->
    case get_record_for_id(SessionId) of
        not_found -> {error, not_found};
        {ok, #erwa_session_record{realm=Realm}} -> {ok, Realm}
    end.

-spec add_subscription(SubscriptionId::non_neg_integer(), SessionId::non_neg_integer()) -> ok | {error, atom()}.
add_subscription(SubscriptionId, SessionId) ->
    {atomic, Res} =
	mnesia:transaction(mnes_addRemSub(SubscriptionId,SessionId,add)),
    Res.

-spec rem_subscription(SubscriptionId::non_neg_integer(),
					   SessionId::non_neg_integer()) -> ok | {error, atom()}.
rem_subscription(SubscriptionId, SessionId) ->
	{atomic, Res} =
	mnesia:transaction(mnes_addRemSub(SubscriptionId,SessionId,del)),
	Res.	

mnes_addRemSub(SubscriptionId,SessionId,Add) ->
	F = fun () -> 
			case mnesia:read(erwa_session_record, SessionId) of
				[] -> {error, not_found};
				[#erwa_session_record{subscriptions=Subs}=Sess] ->
					Subs2 = lists:delete(SubscriptionId, Subs),
					NewSubs = case Add of 
								  add -> [SubscriptionId | Subs2];
								  _ -> Subs2
							  end,
					ok =
					mnesia:write(Sess#erwa_session_record{subscriptions=NewSubs}),
					ok
			end 
		end,
	F.



-spec get_subscriptions(SessionId::non_neg_integer()) -> [non_neg_integer()] .
get_subscriptions(SessionId) ->
    case get_record_for_id(SessionId) of 
        not_found -> [];
        {ok, Sess} -> Sess#erwa_session_record.subscriptions
    end.

-spec clear_subscriptions(SessionId::non_neg_integer()) -> ok .
clear_subscriptions(SessionId) ->
    ClearSubs = fun() -> 
                        case mnesia:read(erwa_session_record, SessionId) of
                            [] -> ok;
                            [#erwa_session_record{}=Sess] ->
                                ok = mnesia:write(Sess#erwa_session_record{subscriptions=[]}),
                                ok
                        end 
                end,
    {atomic, Res} = mnesia:transaction(ClearSubs) ,
    Res.

-spec add_registration(RegistrationId::non_neg_integer(), SessionId::non_neg_integer()) -> ok | {error, atom()}.
add_registration(RegistrationId, SessionId) -> 
    {atomic, Res} =
	mnesia:transaction(mnes_addRemReg(RegistrationId,SessionId,add)),
    Res.


-spec rem_registration(RegistrationId::non_neg_integer(),
					   SessionId::non_neg_integer()) -> ok | {error, atom()}.
rem_registration(RegistrationId, SessionId) ->
	{atomic, Res} =
	mnesia:transaction(mnes_addRemReg(RegistrationId,SessionId,del)),
	Res.	

-spec clear_registrations(SessionId::non_neg_integer()) -> ok.
clear_registrations(SessionId) ->
    ClearRegs = fun() ->
                        case mnesia:read(erwa_session_record, SessionId) of
                            [] -> ok;
                            [#erwa_session_record{} = Sess] ->
                                ok = mnesia:write(Sess#erwa_session_record{registrations=[]}),
                                ok
                        end 
                end,
    {atomic, Res} = mnesia:transaction(ClearRegs),
    Res.

-spec get_registrations(SessionId::non_neg_integer()) -> [non_neg_integer()] .
get_registrations(SessionId) ->
    case get_record_for_id(SessionId) of 
        not_found -> [];
        {ok, Sess} -> Sess#erwa_session_record.registrations
    end.

mnes_addRemReg(RegistrationId,SessionId,Add) ->
	F = fun () -> 
			case mnesia:read(erwa_session_record, SessionId) of
				[] -> {error, not_found};
				[#erwa_session_record{registrations=Regs}=Sess] ->
					Regs2 = lists:delete(RegistrationId, Regs),
					NewRegs = case Add of 
								  add -> [RegistrationId | Regs2];
								  _ -> Regs2
							  end,
					ok =
					mnesia:write(Sess#erwa_session_record{registrations=NewRegs}),
					ok
			end 
		end,
	F.



-spec get_session_count(Realm::binary()) -> {ok, non_neg_integer()} | {error, Reason::term()}.
get_session_count(Realm) ->
    F = fun() ->
                Records =  mnesia:index_read(erwa_session_record, Realm, realm),
                length(Records)
        end,
    {atomic, Result} = mnesia:transaction(F),
    {ok, Result}.

-spec get_session_ids(Realm::binary()) -> {ok,[non_neg_integer()]} | {error, Reason::term()}.
get_session_ids(Realm) ->
    F = fun() ->
                mnesia:index_read(erwa_session_record, Realm, realm)
        end,
    {atomic, Records} = mnesia:transaction(F),
    ExtractId = fun(#erwa_session_record{id=ID},List) ->
                        [ID|List]
                end,
    {ok,lists:foldl(ExtractId,[],Records)}.


add_session(Pid,Realm) ->
  ID = crypto:rand_uniform(0,9007199254740992),
	Data = #erwa_session_record{id=ID, pid=Pid, realm=Realm},
    AddSession = fun() -> case mnesia:read({erwa_session_record, ID}) of 
                              [] -> mnesia:write(Data);
                              _ -> mnesia:abort(already_exists)
                          end 
                 end,
	case mnesia:transaction(AddSession) of
		{atomic, ok} ->
            publish_metaevent(on_join,#{realm => Realm, session => ID}, Realm),
            ID;
		{aborted, already_exists}  -> 
            add_session(Pid,Realm)
	end.


delete_session(Pid) when is_pid(Pid) ->
    RemoveSession = fun() ->
                            case mnesia:index_read(erwa_session_record, Pid, pid)	of 
                                [] -> {atomic, not_found};
                                [#erwa_session_record{id=SessionId,realm=Realm}] -> mnesia:delete({erwa_session_record,
                                                            SessionId}),
                                             {ok,SessionId,Realm}
                            end 
                    end,
    case mnesia:transaction(RemoveSession) of
        {atomic, {ok, SessionId,Realm}} -> 
            publish_metaevent(on_leave,SessionId,Realm),
            ok;
        _ -> not_found
    end;
delete_session(ID) ->
    RemoveSession = fun() -> mnesia:delete({erwa_session_record, ID}) end,
    case mnesia:transaction(RemoveSession) of
        {atomic, ok} -> 
            ok;
        _ ->
            not_found
    end.

get_record_for_id(SessionId) ->
    F = fun() -> mnesia:read(erwa_session_record, SessionId) end,
    case mnesia:transaction(F) of
        {atomic,[]} -> not_found;
        {atomic,[El]} -> {ok,El}
    end.

publish_metaevent(Event,Arg,Realm) ->
    MetaTopic = case Event of 
                    on_join -> <<"wamp.session.on_join">>;
                    on_leave -> <<"wamp.session.on_leave">>
                end, 
    erwa_broker:publish(MetaTopic,#{},[Arg],undefined,no_session,Realm).

-ifdef(TEST).

simple_test() ->
    mnesia:start(),
    create_table(),
    erwa_publications:create_table(),
    erwa_realms:init(),
    erwa_realms:add(<<"test_realm">>),
	ok = create_table(),
	not_found = unregister_session(), 
  {ok,_} = register_session(<<"test_realm">>),
  ok = unregister_session(),
	ok = drop_table().
-endif.

