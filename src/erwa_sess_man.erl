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

-export([create_session/0]).
-export([connect_to/1]).
-export([unregister_session/0]).
-export([get_realm/1]).

-export([set_transport/1]).
-export([set_authdata/1]).
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
                    authid = anonymous,
					pid = unknown,
                    role = none,
					realm = none,
                    authmethod = none,
                    authprovider = none,
                    transport = unknown,
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

-spec create_session() -> {ok,non_neg_integer()}.
create_session() ->
    Pid = self(),
    AddSession = fun() -> 
                         ID = ensure_unique_id(),
                         Data = #erwa_session_record{id=ID, pid=Pid},
                         ok = mnesia:write(Data),
                         {ok,ID}
                 end,
    {atomic, Res} =  mnesia:transaction(AddSession),
    Res.

ensure_unique_id() ->
    %%ID = crypto:rand_uniform(0,9007199254740993),
	ID = rand:uniform(9007199254740993),
    case mnesia:read({erwa_session_record,ID}) of
        [] -> ID;
        _ -> ensure_unique_id()
    end.


set_transport(Dict) ->
    Pid = self(),
    {atomic, Res} = mensia:transaction(update_session(Pid,[{transport,Dict}])),
    Res.


set_authdata(Map) ->
    Pid = self(),
    {atomic, Res} = mensia:transaction(update_session(Pid,Map)),
    Res.

update_session(Pid, Map) when is_map(Map) ->
    update_session(Pid, maps:to_list(Map));
update_session(Pid, PropList) ->
    case mnesia:index_read(erwa_session_record,Pid, pid) of
        [#erwa_session_record{} = Session] ->
            ok = mnesia:write(update_session_records(Session, PropList)),
            ok;
        _ ->  {error, not_found}
    end.

update_session_records(Sess, []) ->
    Sess;
update_session_records(Sess, [{transport, Trans} | Rest ] ) ->
    update_session_records(Sess#erwa_session_record{transport = Trans}, Rest);
update_session_records(Sess, [{authid, AuthId}| Rest]) ->
    update_session_records(Sess#erwa_session_record{authid = AuthId}, Rest);
update_session_records(Sess, [{role, Role} | Rest]) ->
    update_session_records(Sess#erwa_session_record{role = Role}, Rest);
update_session_records(Sess, [{authmethod, Method} | Rest]) ->
    update_session_records(Sess#erwa_session_record{authmethod = Method}, Rest);
update_session_records(Sess, [{authprovider, Provider} | Rest ]) ->
    update_session_records(Sess#erwa_session_record{authprovider = Provider}, Rest);
update_session_records(Sess, [_|Rest]) ->
    update_session_records(Sess, Rest). 



-spec connect_to(Realm :: binary()) -> ok | {error, term()}.
connect_to(Realm) ->
    Pid = self(),
    case erwa_realms:exists(Realm) of
        true -> session_to_realm(Pid, Realm);
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


session_to_realm(Pid, Realm) ->
    Connect = fun() ->
                      case mnesia:index_read(erwa_session_record,Pid, pid) of
                          [#erwa_session_record{realm = none} = Session] ->
                              ok = mnesia:write(Session#erwa_session_record{realm=Realm}),
                              {ok, Session};
                          _ ->  {error, not_found}
                      end 
              end,  
    {atomic, Res} = mnesia:transaction(Connect),
    case Res of 
        {ok,Session} ->
            Info = session_to_dict(Session),
            publish_metaevent(on_join,maps:with([id,authid,role,authmethod,authprovider,transport],Info), Realm),
            ok;
        Res -> Res 
    end.

session_to_dict(Session) ->
    #erwa_session_record{
       id = Id,
       authid = AuthId,
       pid = Pid,
       role = Role,
       realm = Realm,
       authmethod = AuthMethod,
       authprovider = AuthProvider,
       transport = Transport,
       subscriptions = Subs,
       registrations = Regs 
      } = Session,
    #{ id => Id,
       authid => AuthId,
       pid => Pid,
       role => Role,
       realm => Realm,
       authmethod => AuthMethod,
       authprovider => AuthProvider,
       transport => Transport,
       subscriptions => Subs,
       registrations => Regs
     }.


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

%% -ifdef(TEST).
%%
%% simple_test() ->
%%     mnesia:start(),
%%     create_table(),
%%     erwa_publications:create_table(),
%%     erwa_realms:init(),
%%     erwa_realms:add(<<"test_realm">>),
%% 	ok = create_table(),
%% 	not_found = u_nregister_session(), 
%%     {ok,_} = create_session(),
%%   ok = unregister_session(),
%% 	ok = drop_table().
%% -endif.

