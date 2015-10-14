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

-module(erwa_dealer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



%% API
-export([init/1]).
-export([cleanup/1]).

-export([register/4]).
-export([unregister/3]).
-export([unregister_all/2]).

-export([call/7]).

-export([get_registrations/1]).
-export([get_registration/2]).

-record(erwa_procedure,{
		  id = none,
		  uri = none,
		  created = unknown,
		  match = exact,
		  invoke = single,
		  options = [],
          ids = [],
          last = 0 
         }).



-spec init( Realm :: binary() ) -> ok.
init(Realm) ->
	create_table_for_realm(Realm).

-spec cleanup( Realm :: binary()  ) -> ok.
cleanup(Realm) ->
	delete_table_for_realm(Realm).

-spec get_registrations( Realm :: binary() ) -> map().
get_registrations(Realm) ->
	Tab = realm_to_table_name(Realm),
	GetRegistrations = fun() ->
							   E = mnesia:index_read(Tab, exact, match),
							   P = mnesia:index_read(Tab, prefix, match),
							   W = mnesia:index_read(Tab, wildcard, match),
							   {E, P, W}
					   end,

	{atomic, {ExactRecs, PrefixRecs, WildcardRecs}}	= mnesia:transaction(GetRegistrations),
	 Filter = fun(#erwa_procedure{id=Id,uri=Uri},List ) ->
             case binary:part(Uri,{0,5}) == <<"wamp.">> of
               true ->
                 List;
               false ->
                 [Id | List]
             end
           end,
	Exact = lists:foldl(Filter,[],ExactRecs),
    Prefix = lists:foldl(Filter,[],PrefixRecs),
    Wildcard = lists:foldl(Filter,[],WildcardRecs),
	{ok,#{exact => Exact, prefix => Prefix, wildcard => Wildcard}}.



-spec get_registration( non_neg_integer(), binary() ) -> map().
get_registration(RegistrationId, Realm) ->
	Tab = realm_to_table_name(Realm),
	GetReg = fun() ->
					 mnesia:index_read(Tab,RegistrationId,id)
			 end,
	case mnesia:transaction(GetReg) of 
		{atomic, []} -> 
			{error, not_found};
        {atomic, [#erwa_procedure{uri=Uri, invoke=Invoke, id=Id, match=Match,
                             created=Created}]} ->
            {ok,#{uri => Uri, invoke => Invoke, id => Id, match => Match,
                  created => Created}}
    end.



-spec register(ProcedureUri :: binary(), Options :: map(),
			   SessionId::non_neg_integer(), Realm :: binary() ) -> { ok, RegistrationId :: non_neg_integer() }.
register(ProcedureUri, Options, SessionId, Realm) ->
    Args = get_register_args(ProcedureUri, SessionId, Options, Realm),	
    register_procedure(Args).


register_procedure(#{match := exact} = Args) ->
    send_metaevents(register_exact_procedure(Args));
register_procedure(_) ->
    {error, not_supported}.

send_metaevents({ok,created,#{id := Id} = Args}) ->
    publish_metaevent(on_create,Args),
    publish_metaevent(on_register,Args),
    {ok,Id};
send_metaevents({ok,registered, #{id := Id} = Args}) ->
    publish_metaevent(on_register,Args),
    {ok, Id};
send_metaevents({ok,unregistered,Args}) ->
    publish_metaevent(on_unregister,Args),
    ok;
send_metaevents({ok,deleted,Args}) ->
    publish_metaevent(on_unregister,Args),
    publish_metaevent(on_delete,Args),
    ok.


register_exact_procedure(Args) ->
    true = is_valid_invoke(Args),
    %Invoke = maps:get(invoke,Args),
    F = fun() ->
                case can_uri_be_used(Args) of
                    false -> 
                        {error, procedure_already_exists};
                    true ->
                        {ok, Created, ProcId} = create_or_update_procedure(get_procedure(Args)),
                        SessionId = maps:get(session, Args),
                        ok = erwa_sess_man:add_registration(ProcId, SessionId),
                        {ok, Created, ProcId}
                end 
        end, 
    {atomic, Res} = mnesia:transaction(F),
    Res.

is_valid_invoke(#{invoke := Invoke}) ->
    lists:member(Invoke,[single,roundrobin,random,first,last]).


can_uri_be_used(#{ uri := Uri, realm := Realm, invoke := Invoke }) -> 
    Table = realm_to_table_name(Realm),
    is_resusable_procedure(mnesia:index_read(Table,Uri,uri),Invoke).


is_resusable_procedure([],_) ->
    true;
is_resusable_procedure(#erwa_procedure{invoke=single},_) ->
    false;
is_resusable_procedure(#erwa_procedure{invoke=Invoke},Invoke) ->
    true;
is_resusable_procedure(_,_) ->
    false.


get_procedure(#{realm := Realm, uri:= Uri } = Args) ->
    Table = realm_to_table_name(Realm),
    {mnesia:index_read(Table,Uri,uri), Args}.

create_or_update_procedure({[#erwa_procedure{id=Id, ids=SessionIds, last=Last}=Proc],#{realm := Realm, session := SessionId}=Args}) ->
    Table = realm_to_table_name(Realm),
    {NewSessionIds, NewLast} =  case lists:member(SessionId,SessionIds) of
                                    true -> {SessionIds, Last};
                                    false -> {[SessionId | SessionIds], Last+1}
                                end,
    ok = mnesia:write(Table,Proc#erwa_procedure{ids=NewSessionIds, last=NewLast}, write),
    {ok, registered, Args#{id := Id}};
create_or_update_procedure({[],ArgsIn}) ->
    Args = ensure_unique_id(ArgsIn), 
    Table = realm_to_table_name(maps:get(realm,Args)),
    ok = mnesia:write(Table,register_args_to_record(Args),write),
    {ok, created, Args}.

ensure_unique_id(Args) ->
    Id = gen_id(),
    Table = realm_to_table_name(maps:get(realm,Args)),
    case mnesia:read(Table, Id) of 
        [] -> Args#{id := Id};
        _ -> ensure_unique_id(Args)
    end.


register_args_to_record(Args) ->
    #{ uri := Uri,  created := Created, id := Id, 
       session := SessionId, invoke := Invoke, match := Match } = Args,
    #erwa_procedure{id =Id,
                    uri = Uri,
                    created = Created,
                    match = Match,
                    invoke = Invoke,
                    ids = [SessionId]
                   }.


get_register_args(ProcedureUri, SessionId, Options, Realm) ->
    #{
                                                      id => none,
                                                      invoke => maps:get(invoke, Options, single),
                                                      match => maps:get(match, Options, exact),
                                                      created => calendar:universal_time(),
                                                      uri => ProcedureUri,
                                                      realm => Realm,
                                                      session => SessionId
                                                     }.


-spec unregister(RegistrationId :: non_neg_integer(), SessionId ::
				 non_neg_integer(), Realm :: binary() ) -> ok.
unregister( RegistrationId,SessionId, Realm) ->
    Table = realm_to_table_name(Realm),
    F = fun() ->
                case mnesia:read(Table,RegistrationId) of
                    [] ->
                        mnesia:abort(not_found);
                    [#erwa_procedure{id=ProcId, ids=Ids, uri=Uri} = Proc ] ->
                        case lists:member(SessionId,Ids) of
                            true -> 
                                ok = erwa_sess_man:rem_registration(RegistrationId, SessionId),
                                NewIds = lists:delete(SessionId, Ids),
                                Args = #{uri => Uri, id => ProcId, session => SessionId, realm => Realm},
                                case NewIds of 
                                    [] ->
                                        ok = mnesia:delete(Table,ProcId,write),
                                        {ok,deleted,Args};
                                        %% publish_metaevent(on_unregister,Uri,ProcId,SessionId,Realm,undefined),
                                        %% publish_metaevent(on_delete,Uri,ProcId,SessionId,Realm,undefined);
                                    _ -> 
                                        ok = mnesia:write(Table, Proc#erwa_procedure{ids=NewIds}, write),
                                        {ok,unregistered,Args}
                                        %% publish_metaevent(on_unregister,Uri,ProcId,SessionId,Realm,undefined),
                                end;
                            false ->
                                {error, not_registered}
                        end 
                end 
        end,
    {atomic, Res} =  mnesia:transaction(F),
    send_metaevents(Res).



-spec unregister_all( SessionId::non_neg_integer(), Realm :: binary() ) -> ok.
unregister_all( SessionId, Realm ) ->
    RegistrationIds = erwa_sess_man:get_registrations(SessionId),
    unregister_all(RegistrationIds,SessionId,Realm). 

unregister_all([],_SessionId, _Realm) ->
    ok;
unregister_all([H|T],SessionId, Realm) ->
    ok = unregister(H, SessionId, Realm),
    unregister_all(T,SessionId,Realm).


-spec call(Uri :: binary(), RequestId :: non_neg_integer(), Options :: map(),
           Arguments :: list(), ArgumentsKw :: map(), Session::term(),
		   Realm::binary()) ->
  {ok, pid()} | {error,invocation_failed} | {error, procedure_not_found}.
call(Uri, RequestId, Options, Arguments, ArgumentsKw, SessionId, Realm) ->
    start_call(get_call_params(Uri, Realm, RequestId, SessionId, Options, Arguments, ArgumentsKw)).

start_call ({error, _} = Error) ->
    Error;
start_call({ok, CallInfo}) ->
    case erwa_invocation:perform(CallInfo) of
        {ok,Pid} ->
          {ok,Pid};
        _ -> {error,invocation_failed}
    end.

get_call_params(Uri, Realm, RequestId, SessionId,Options,Arguments,ArgumentsKw) ->
    Table = realm_to_table_name(Realm),
    F = fun() ->
                case mnesia:index_read(Table, Uri, uri) of
                    [] -> 
                        {error, procedure_not_found};
                    [#erwa_procedure{uri = Uri, id = Id, ids = Ids, last=L, invoke = Invoke}=Proc] ->
                        Callee = get_callee(Ids, Invoke, L+1 ),
                        ok = update_last_if_needed(Proc, Invoke, Callee, Realm),
                        {ok, #{procedure_id => Id,
                               caller_id => SessionId,
                               call_req_id => RequestId,
                               call_options => Options,
                               call_arguments => Arguments,
                               call_argumentskw => ArgumentsKw,
                               callee_ids => [Callee],
                               realm => Realm
                              }}
                        
                end 
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

get_callee([H|_],single,_) ->
    H;
get_callee(Callees,random,_) ->
    Len = length(Callees),
    lists:nth(crypto:rand_uniform(1,Len),Callees);
get_callee(Callees,roundrobin,NextIn) ->
    Next = case NextIn > length(Callees) of 
               true ->
                   1;
               false ->
                   NextIn
           end,
    lists:nth(Next,Callees);
get_callee([H|_],first,_) ->
    H;
get_callee(Callees,last,_) -> 
    [H|_] = lists:reverse(Callees),
    H.

update_last_if_needed(#erwa_procedure{ids = Callees} = Proc, roundrobin, Callee, Realm) ->
    Table = realm_to_table_name(Realm),
    Last = get_last(Callee, Callees),
    ok = mnesia:write(Table,Proc#erwa_procedure{last=Last},write);
update_last_if_needed(_,_,_,_) ->
    ok.


get_last(Callee, Callees) ->
    get_last(Callee, Callees, 1).

get_last(_Callee, [], _Pos) -> 1;
get_last(Callee, [Callee|_], Pos) -> Pos;
get_last(Callee, [_|T], Pos ) -> get_last(Callee, T, Pos +1).
    

create_table_for_realm(Realm) ->
	Table = realm_to_table_name(Realm),
	case lists:member(Table, mnesia:system_info(local_tables)) of
		true ->
			mnesia:delete_table(Table);
		_-> do_nothing
	end,
	{atomic, ok} = mnesia:create_table(Table, [{disc_copies, []},
														   {ram_copies,
															[node()]}, 
														   {type, set},
														   {record_name,
															erwa_procedure},
														   {attributes,
															record_info(fields,
																		erwa_procedure)},{index,[uri,match]}]),
	ok.


delete_table_for_realm(Realm) ->
	{atomic, ok} = mnesia:delete_table(realm_to_table_name(Realm)),
	ok.


realm_to_table_name(Realm) ->
	Prefix = <<"erwa_proc_">>,
	binary_to_atom(<< Prefix/binary, Realm/binary >>, utf8 ).


gen_id() ->
  crypto:rand_uniform(0,9007199254740992).

publish_metaevent(Event,Args) ->
    #{uri := ProcedureUri, id := ProcId, session := SessionId, realm := Realm} = Args,
    case binary:part(ProcedureUri,{0,5}) == <<"wamp.">> of
        true ->
            % do not fire metaevents on "wamp.*" uris
            ok;
        false ->
            {MetaTopic, SecondArg} = case Event of
                                         on_create -> 
                                             {<<"wamp.registration.on_create">>,
                                              maps:with([id,created,uri,match,invoke],Args)};
                                         on_register -> 
                                             {<<"wamp.registration.on_register">>,
                                              ProcId};
                                         on_unregister -> 
                                             {<<"wamp.registration.on_unregister">>,
                                              ProcId};
                                         on_delete -> 
                                             {<<"wamp.registration.on_delete">>,
                                              ProcId}
                                     end,
            {ok,_} = erwa_broker:publish(MetaTopic,#{},[SessionId,SecondArg],undefined,no_session,Realm)
    end,
    ok.


%*************************************************************************************************
%********************************          TESTS              ************************************
%*************************************************************************************************

%% -ifdef(TEST).
%%
%% flush() ->
%%   receive
%%     _ ->
%%       flush()
%%   after 0 ->
%%     ok
%%   end.
%%
%% get_tablesize(#data{ets=Ets}) ->
%%   ets:info(Ets,size).
%%
%% ensure_tablesize(_Number,_Data,MaxTime) when MaxTime =< 0 ->
%%   timeout;
%% ensure_tablesize(Number,Data,MaxTime) ->
%%   case get_tablesize(Data) of
%%     Number -> ok;
%%     _ ->
%%       receive
%%       after 10 -> ok
%%       end,
%%       NewTime = MaxTime - 10,
%%       ensure_tablesize(Number,Data,NewTime)
%%   end.
%%
%% start_stop_test() ->
%%   {ok,Pid} = start(),
%%   {ok,stopped} = stop(Pid).
%%
%% set_metaevents_test() ->
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%   enable_metaevents(Data),
%%   disable_metaevents(Data),
%%   {ok,stopped} = stop(Data).
%%
%% un_register_test() ->
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%   SessionId = 123,
%%   0 = get_tablesize(Data),
%%   {ok,ID1} = register(<<"proc.test1">>,#{},SessionId,Data),
%%   3 = get_tablesize(Data),
%%   {ok,ID2} = register(<<"proc.test2">>,#{},SessionId,Data),
%%   5 = get_tablesize(Data),
%%   ok = unregister(ID1,SessionId,Data),
%%   3 = get_tablesize(Data),
%%   {error,not_found} = unregister(ID1,SessionId,Data),
%%   ok = unregister(ID2,SessionId,Data),
%%   0 = get_tablesize(Data),
%%   {error,not_found} = unregister(ID2,SessionId,Data),
%%   0 = get_tablesize(Data),
%%   {ok,stopped} = stop(Data).
%%
%% unregister_all_test() ->
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%   SessionId = 123,
%%   0 = get_tablesize(Data),
%%   ok = unregister_all(SessionId,Data),
%%   0 = get_tablesize(Data),
%%   {ok,ID1} = register(<<"proc.test1">>,#{},SessionId,Data),
%%   3 = get_tablesize(Data),
%%   {ok,ID2} = register(<<"proc.test2">>,#{},SessionId,Data),
%%   5 = get_tablesize(Data),
%%   ok = unregister_all(SessionId,Data),
%%   0 = get_tablesize(Data),
%%   {error,not_found} = unregister(ID1,SessionId,Data),
%%   0 = get_tablesize(Data),
%%   {error,not_found} = unregister(ID2,SessionId,Data),
%%   0 = get_tablesize(Data),
%%   ok = unregister_all(SessionId,Data),
%%   0 = get_tablesize(Data),
%%   {ok,stopped} = stop(Data).
%%
%% multiple_un_register_test() ->
%%   flush(),
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%   SessionId = 123,
%%   0 = get_tablesize(Data),
%%   {ok,ID1} = register(<<"proc.test1">>,#{},SessionId,Data),
%%   % procedure       x 1
%%   % id_procedure    x 1
%%   % id_info         x 1
%%   3 = get_tablesize(Data),
%%   {ok,ID2} = register(<<"proc.test2">>,#{},SessionId,Data),
%%   % procedure       x 2
%%   % id_procedure    x 2
%%   % id_info         x 1
%%   5 = get_tablesize(Data),
%%   MyPid = self(),
%%   F = fun() ->
%%         {error,procedure_already_exists} = erwa_dealer:register(<<"proc.test1">>,#{},456,Data),
%%         MyPid ! error_received,
%%         ok = receive
%%                try_again -> ok
%%              end,
%%         {ok,_} = erwa_dealer:register(<<"proc.test1">>,#{},456,Data),
%%         MyPid ! second_subscription_passed,
%%         ok = receive
%%                clean -> ok
%%              end,
%%         ok = erwa_dealer:unregister_all(456,Data),
%%         MyPid ! done,
%%         ok
%%       end,
%%   CPid = spawn(F),
%%   receive
%%     error_received ->
%%       ok
%%   end,
%%   % procedure       x 2
%%   % id_procedure    x 2
%%   % id_info         x 1
%%   5 = get_tablesize(Data),
%%   ok = unregister(ID1,SessionId,Data),
%%   % procedure       x 1
%%   % id_procedure    x 1
%%   % id_info         x 1
%%   3 = get_tablesize(Data),
%%   CPid ! try_again,
%%   ok = receive
%%          second_subscription_passed -> ok
%%        end,
%%   % procedure       x 2
%%   % id_procedure    x 2
%%   % id_info         x 2
%%   6 = get_tablesize(Data),
%%   CPid ! clean,
%%   ok = receive
%%          done -> ok
%%        end,
%%   % procedure       x 1
%%   % id_procedure    x 1
%%   % id_info         x 1
%%   ok = ensure_tablesize(3,Data,1000),
%%   ok = unregister(ID2,SessionId,Data),
%%   % procedure       x 0
%%   % id_procedure    x 0
%%   % id_info         x 0
%%   0 = get_tablesize(Data),
%%   ok = unregister_all(SessionId,Data),
%%   % procedure       x 0
%%   % id_procedure    x 0
%%   % id_info         x 0
%%   0 = get_tablesize(Data),
%%   flush(),
%%   {ok,stopped} = stop(Data).
%%
%%
%% call_test() ->
%%   erwa_invocation_sup:start_link(),
%%   erwa_sess_man:create_table(),
%%   flush(),
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%   Realm = <<"erwa.test">>,
%%   MyPid = self(),
%%   F = fun() ->
%%         {ok,SessionId} = erwa_sess_man:register_session(Realm),
%%         {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},SessionId,Data),
%%         MyPid ! subscribed,
%%         {ok,A,B,InvocationPid} = receive
%%                                    {erwa,{invocation,set_request_id,ProcId,#{invocation_pid := InvPid},[In1,In2],undefined}} ->
%%                                      {ok,In1,In2,InvPid}
%%                                  end,
%%         ok = erwa_invocation:yield(InvocationPid,#{},[A+B],undefined,SessionId),
%%         ok = erwa_dealer:unregister_all(SessionId,Data),
%%         ok
%%       end,
%%   CPid = spawn(F),
%%   {ok,SessionId} = erwa_sess_man:register_session(Realm),
%%   monitor(process,CPid),
%%   ok = receive
%%          subscribed -> ok
%%        end,
%%   RequestId = gen_id(),
%%   A = gen_id(),
%%   B = gen_id(),
%%   C = A+B,
%%   {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{}, [A,B], undefined,SessionId,Data),
%%   monitor(process, InvocationPid),
%%   ok = receive
%%          {erwa,{result, RequestId, #{}, [C], undefined}} -> ok
%%        end,
%%
%%   ok = receive
%%            {'DOWN',_,process,CPid,normal} ->
%%              ok
%%            end,
%%   ok = receive
%%          {'DOWN',_,process,InvocationPid,_} ->
%%            ok
%%        end,
%%   flush(),
%%   erwa_sess_man:drop_table(),
%%   ok.
%%
%%
%% caller_identification_test() ->
%%   erwa_invocation_sup:start_link(),
%%   erwa_sess_man:create_table(),
%%   flush(),
%%   Realm = <<"erwa.test">>,
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%   {ok,SessionId} = erwa_sess_man:register_session(Realm),
%%   MyPid = self(),
%%   F = fun() ->
%%         {ok,LocalSessId} = erwa_sess_man:register_session(Realm),
%%         {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},LocalSessId,Data),
%%         MyPid ! subscribed,
%%         {ok,A,B,InOptions} = receive
%%                                    {erwa,{invocation,set_request_id,ProcId,Opts,[In1,In2],undefined}} ->
%%                                      {ok,In1,In2,Opts}
%%                                  end,
%%         SessionId = maps:get(caller,InOptions),
%%         InvocationPid = maps:get(invocation_pid,InOptions),
%%         ok = erwa_invocation:yield(InvocationPid,#{},[A+B],undefined,LocalSessId),
%%         ok = erwa_dealer:unregister_all(LocalSessId,Data),
%%         receive
%%           after 100 ->
%%             MyPid ! done
%%         end,
%%         ok
%%       end,
%%   spawn(F),
%%
%%   ok = receive
%%          subscribed -> ok
%%        end,
%%   RequestId = gen_id(),
%%   A = gen_id(),
%%   B = gen_id(),
%%   C = A+B,
%%   {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{disclose_me => true }, [A,B], undefined,SessionId,Data),
%%   monitor(process, InvocationPid),
%%   ok = receive
%%          {erwa,{result, RequestId, #{}, [C], undefined}} -> ok
%%        end,
%%   ok = receive
%%          {'DOWN',_,process,InvocationPid,_} ->
%%            ok
%%        end,
%%   ok = receive
%%          done -> ok
%%        end,
%%   flush(),
%%   erwa_sess_man:drop_table(),
%%   ok.
%%
%%
%% call_cancel_test() ->
%%   erwa_invocation_sup:start_link(),
%%   erwa_sess_man:create_table(),
%%   flush(),
%%   Realm = <<"erwa.test">>,
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%
%%   MyPid = self(),
%%   F = fun() ->
%%         {ok,SessionId} = erwa_sess_man:register_session(Realm),
%%         {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},SessionId,Data),
%%         MyPid ! subscribed,
%%         {ok,InOptions} = receive
%%                            {erwa,{invocation,set_request_id,ProcId,Opts,_,_}} ->
%%                              {ok,Opts}
%%                          end,
%%         InvocationPid = maps:get(invocation_pid,InOptions),
%%         ok = receive
%%                {erwa,{interrupt,set_request_id,#{invocation_pid := InvocationPid}}} ->
%%                  ok
%%              end,
%%         ok = erwa_invocation:error(InvocationPid,#{},canceled,undefined,undefined,SessionId),
%%         ok = erwa_dealer:unregister_all(SessionId,Data),
%%         receive
%%           after 100 ->
%%             MyPid ! done
%%         end,
%%         ok
%%       end,
%%   spawn(F),
%%   {ok,SessionId} = erwa_sess_man:register_session(Realm),
%%   ok = receive
%%          subscribed -> ok
%%        end,
%%   RequestId = gen_id(),
%%   A = gen_id(),
%%   B = gen_id(),
%%   {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{}, [A,B], undefined,SessionId,Data),
%%   monitor(process, InvocationPid),
%%   receive
%%     after 100 ->
%%       ok
%%   end,
%%   erwa_invocation:cancel(InvocationPid,[]),
%%   ok = receive
%%          {erwa,{error,call,_,_,_,_,_}} -> ok
%%        end,
%%   ok = receive
%%          {'DOWN',_,process,InvocationPid,_} ->
%%            ok
%%        end,
%%   ok = receive
%%          done -> ok
%%        end,
%%   flush(),
%%   erwa_sess_man:drop_table(),
%%   ok.
%%
%%
%% call_progressive_test() ->
%%   erwa_invocation_sup:start_link(),
%%   erwa_sess_man:create_table(),
%%   flush(),
%%   Realm = <<"erwa.test">>,
%%   {ok,Pid} = start(),
%%   {ok,Data} = get_data(Pid),
%%
%%   MyPid = self(),
%%   F = fun() ->
%%         {ok,SessionId} = erwa_sess_man:register_session(Realm),
%%         {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},SessionId,Data),
%%         MyPid ! subscribed,
%%         {ok,InOptions} = receive
%%                            {erwa,{invocation,set_request_id,ProcId,Opts,_,_}} ->
%%                              {ok,Opts}
%%                          end,
%%         InvocationPid = maps:get(invocation_pid,InOptions),
%%         ok = erwa_invocation:yield(InvocationPid,#{progress => true},[234],undefined,SessionId),
%%         receive
%%           after 50 ->
%%             ok
%%         end,
%%         ok = erwa_invocation:yield(InvocationPid,#{},[567],undefined,SessionId),
%%         ok = erwa_dealer:unregister_all(SessionId,Data),
%%         ok
%%       end,
%%   spawn(F),
%%   {ok,SessionId} = erwa_sess_man:register_session(Realm),
%%   ok = receive
%%          subscribed -> ok
%%        end,
%%   RequestId = gen_id(),
%%   A = gen_id(),
%%   B = gen_id(),
%%   {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{receive_progress => true}, [A,B], undefined,SessionId,Data),
%%   monitor(process, InvocationPid),
%%   ok = receive
%%          {erwa,{result, RequestId, #{progress := true}, [234], undefined}} -> ok
%%        end,
%%   ok = receive
%%          {erwa,{result, RequestId, #{}, [567], undefined}} -> ok
%%        end,
%%   ok = receive
%%          {'DOWN',_,process,InvocationPid,_} ->
%%            ok
%%        end,
%%   flush(),
%% 	erwa_sess_man:drop_table(),
%%   ok.
%%
%% garbage_test() ->
%%   {ok,Pid} = start(),
%%   ignored = gen_server:call(Pid,some_garbage),
%%   ok = gen_server:cast(Pid,some_garbage),
%%   Pid ! some_garbage,
%%   {ok,stopped} = stop(Pid).
%%
%%
%% -endif.
