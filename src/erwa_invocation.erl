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

-module(erwa_invocation).

%% -ifdef(TEST).
%% -include_lib("eunit/include/eunit.hrl").
%% -endif.


%% API
-export([init/1]).
-export([cleanup/1]).

-export([perform/1]).
-export([cancel/3]).
-export([yield/6]).
-export([error/7]).

-export([timeout/2]).

-record(erwa_invocation, {
            id = none,
            realm = realm,
            caller = unknown,
            req_id = unknown,
            callees = [],
            results = [],
            progessive = false,
            timer_ref = none
         }).

init(Realm) ->
    ok = create_table_for_realm(Realm),
    ok.

cleanup(Realm) ->
    ok = delete_table_for_realm(Realm),
    ok.

perform(Args) ->
    {ok, Id} =  create_and_store_invocation(Args),
    ok = send_invocation_message(Id,Args),
    {ok,Id}.


create_and_store_invocation(Args) ->
    #{caller_id := CallerId,
      call_req_id := RequestId,
      call_options := Options,
      callee_ids := Callees,
      realm := Realm} = Args,
    Invocation = #erwa_invocation { 
                    realm = Realm,
                    caller = CallerId,
                    req_id = RequestId,
                    callees = Callees,
                    progessive = progressive(Options)
                   },
    {ok,Id} = store_with_unique_id(Invocation,Realm),
    set_timeout(maps:get(timeout,Options,0),Id,Realm),
    {ok,Id}.

set_timeout(Timeout,Id,Realm) when is_integer(Timeout), Timeout > 0 ->
    timer:apply_after(Timeout,?MODULE, timeout,[Id, Realm]);
set_timeout(_,_,_) ->
    ok.


store_with_unique_id(Invocation, Realm) ->
    Id = gen_id(),
    Table = realm_to_tablename(Realm),
    F = fun() -> 
                case mnesia:read(Table, Id) of 
                    [] -> 
                        ok = mnesia:write(Table, Invocation#erwa_invocation{id=Id}, write),
                        {ok, Id};
                    _ -> 
                        {error, already_exist}
                end 
        end,
    {atomic, Result} = mnesia:transaction(F),
    redo_store_if_failed(Result,Invocation,Realm).

redo_store_if_failed({ok,_} = Result, _, _) ->
    Result;
redo_store_if_failed({error, already_exist},Invocation,Realm) ->
    store_with_unique_id(Invocation, Realm).

progressive(Options) ->
    maps:get(receive_progress,Options,false).

send_invocation_message(Id,Args) ->
    #{procedure_id := ProcedureId,
      call_options := Options,
      call_arguments := Arguments,
      call_argumentskw := ArgumentsKw,
      callee_ids := Callees
      } = Args,
    send_message_to({invocation,Id,ProcedureId,Options,Arguments,ArgumentsKw},Callees).


yield(InvocationId,Options,Arguments,ArgumentsKw,CalleeId,Realm) ->
    ArgumentMap = corral_arguments(Options,Arguments,ArgumentsKw,CalleeId),
    InvocationData = get_invocation(InvocationId,Realm),
    handle_yield_if_valid(is_valid_callee(InvocationData,ArgumentMap),InvocationData,ArgumentMap).


handle_yield_if_valid(true,{ok,InvocationData},ArgumentMap) ->
    Options = maps:get(options,ArgumentMap),
    handle_yield(InvocationData,ArgumentMap,progressive(Options));
handle_yield_if_valid(_,_,_) ->
    ok.


handle_yield(InvocationData,ArgumentMap,true) ->
    handle_progress_message(InvocationData,ArgumentMap);
handle_yield(InvocationData,ArgumentMap,false) ->
    handle_result_message(InvocationData,ArgumentMap).

handle_progress_message(#erwa_invocation{progessive=true}=InvocationData,ArgumentMap) ->
    send_progress_message(ArgumentMap,InvocationData);
handle_progress_message(InvocationData,ArgumentMap) ->
    send_progress_error_message(ArgumentMap,InvocationData),
    delete_invocation(InvocationData).

handle_result_message(InvocationData, ArgumentMap) ->
    send_result_message(ArgumentMap, InvocationData),
    update_or_delete_invocation(ArgumentMap, InvocationData).

send_progress_message(ArgumentMap, #erwa_invocation{req_id=RequestId, caller=CallerId}) ->
    Arguments = maps:get(arguments,ArgumentMap),
    ArgumentsKw = maps:get(arguments_kw, ArgumentMap),
    send_message_to({result, RequestId, #{progress => true}, Arguments, ArgumentsKw},CallerId).

send_progress_error_message(_ArgumentMap, #erwa_invocation{req_id=RequestId, caller=CallerId}) ->
    send_message_to({error,call, RequestId, #{}, <<"erwa.missbehaving_callee">>},CallerId).

send_result_message(ArgumentMap, #erwa_invocation{req_id=RequestId, caller=CallerId}) ->
    Arguments = maps:get(arguments,ArgumentMap),
    ArgumentsKw = maps:get(arguments_kw, ArgumentMap),
    send_message_to({result, RequestId, #{}, Arguments, ArgumentsKw},CallerId).


update_or_delete_invocation(#{callee := CalleeId},#erwa_invocation{id=Id,realm=Realm}) ->
    Table = realm_to_tablename(Realm),
    F = fun() ->
                case mnesia:read(Table,Id) of 
                    [] ->
                        ok;
                    [#erwa_invocation{callees=Callees} = Invocation] ->
                        case lists:delete(CalleeId,Callees) of 
                            [] ->
                                ok = mnesia:delete(Table,Id,write);
                            NewCallees ->
                                ok = mnesia:write(Table,Invocation#erwa_invocation{callees=NewCallees}) 
                        end 
                end 
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.
                        
delete_invocation(#erwa_invocation{id=Id,realm=Realm}) ->
    Table = realm_to_tablename(Realm),
    F = fun() ->
                ok = mnesia:delete(Table,Id,write)
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

corral_arguments(Options, Arguments, ArgumentsKw, CalleeId) ->
    #{ options => Options,
     arguments => Arguments,
     arguments_kw => ArgumentsKw,
     callee => CalleeId}.


get_invocation(InvocationId,Realm) ->
    Table = realm_to_tablename(Realm),
    F = fun() -> 
                case mnesia:read(Table,InvocationId) of 
                    [] -> 
                        {error, not_found};
                    [#erwa_invocation{} = InvocationData] ->
                        {ok, InvocationData};
                    Other -> 
                        {error, Other}
                end 
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.
                   

is_valid_callee({ok,Invocation},Args) ->
    is_valid_callee(Invocation,Args);
is_valid_callee(#erwa_invocation{callees=Callees},#{callee := Callee}) ->
    lists:member(Callee, Callees).


error(InvocationId, Details, ErrorUri, Arguments, ArgumentsKw, CalleeId, Realm) ->
    ArgumentMap = corral_arguments(Details,ErrorUri,Arguments,ArgumentsKw,CalleeId),
    InvocationData = get_invocation(InvocationId,Realm),
    handle_error_if_valid(is_valid_callee(InvocationData,ArgumentMap),InvocationData,ArgumentMap).

handle_error_if_valid(true,{ok,Invocation},ArgumentMap) ->
    send_error_message(ArgumentMap,Invocation),
    update_or_delete_invocation(ArgumentMap,Invocation);
handle_error_if_valid(_,_,_) ->
    ok.


send_error_message(ArgumentMap,#erwa_invocation{caller=CallerId,req_id=RequestId}) ->
    #{details := Details,
     error := ErrorUri,
     arguments := Arguments,
     arguments_kw := ArgumentsKw} = ArgumentMap,    
    send_message_to({error,call,RequestId,Details,ErrorUri,Arguments,ArgumentsKw},CallerId).

corral_arguments(Details, ErrorUri, Arguments, ArgumentsKw, CalleeId) ->
    #{ details => Details,
       error => ErrorUri,
       arguments => Arguments,
       arguments_kw => ArgumentsKw,
       callee => CalleeId}.


cancel(InvocationId, _Options, Realm)  ->
    Invocation = get_invocation(InvocationId, Realm),
    send_interrupt_message(Invocation),
    ok.


timeout(InvocationId, Realm) ->
    Invocation = get_invocation(InvocationId, Realm),
    send_timeout_error(Invocation),
    send_interrupt_message(Invocation),
    ok.

send_interrupt_message({ok,#erwa_invocation{callees=Callees, id=Id}=Invocation}) ->
    send_message_to({interrupt,Id,#{}},Callees),
    delete_invocation(Invocation);
send_interrupt_message(_) ->
    ok.

send_timeout_error({ok,#erwa_invocation{req_id=RequestId, caller=CallerId}}) ->
    send_message_to({error,invocation,RequestId,#{},<<"wamp.error.timeout">>,undefined, undefined},CallerId);
send_timeout_error(_) ->
    ok.

%% handle_cast({error,_Details,ErrorUri,Arguments,ArgumentsKw,CalleeId},#state{caller_id=CallerId,call_req_id=RequestId, callee_ids=Callees}=State) ->
%%   send_message_to({error, call, RequestId, #{}, ErrorUri, Arguments, ArgumentsKw},CallerId),
%%   case lists:delete(CalleeId,Callees) of
%%     [] ->
%%       {stop,normal,State#state{callee_ids=[]}};
%%     NewCallees ->
%%       {noreply,State#state{callee_ids=NewCallees}}
%%   end;
%% handle_cast(_Request, State) ->
%% 	{noreply, State}.
%%
%% handle_info(automatic_cancel, State) ->
%%   NewState = do_cancel(State),
%%   {noreply,NewState};

%% do_cancel(#state{canceled=true}=State) ->
%%   State;
%% do_cancel(#state{callee_ids=Callees}=State) ->
%%   send_message_to({interrupt,set_request_id,#{invocation_pid => self()}},Callees),
%%   timer:send_after(?CANCEL_TIMEOUT,shutdown),
%%   State#state{canceled=true}.
%%
%%
%%     case length(Callees) of
%%       0 ->
%%         {error, no_callees};
%%       1 ->
%%         {ok, State};
%%       _ ->
%%         %multiple callees
%%         %{ok, State}
%%         % error for now
%%         {error,multiple_callees}
%%     end
%%   catch _:Reason ->
%%     {error,Reason}
%%   end.
%%

-spec send_message_to(Msg :: term(), Peer :: list() | non_neg_integer()) -> ok.
send_message_to(Msg,SessionId) when is_integer(SessionId) ->
  send_message_to(Msg,[SessionId]);
send_message_to(Msg,Peers) when is_list(Peers) ->
  Send = fun(SessionId,[]) ->
           erwa_sess_man:send_message_to(Msg,SessionId),
           []
         end,
  lists:foldl(Send,[],Peers),
  ok.



create_table_for_realm(Realm) ->
	Table = realm_to_tablename(Realm),
	case lists:member(Table, mnesia:system_info(local_tables)) of
		true ->
			mnesia:delete_table(Table);
		_-> do_nthing
	end,
    {atomic, ok} = mnesia:create_table(Table, [{disc_copies, []},
                                               {ram_copies, [node()]}, 
                                               {type, set},
                                               {record_name, erwa_invocation},
                                               {attributes, record_info(fields, erwa_invocation)},
                                               {index,[req_id]}
                                              ]),
	ok.

delete_table_for_realm(Realm) ->
	{atomic, ok} = mnesia:delete_table(realm_to_tablename(Realm)),
	ok.


realm_to_tablename(Realm) ->
	Prefix = <<"erwa_invocation_">>,
	binary_to_atom(<< Prefix/binary, Realm/binary >>, utf8 ).

gen_id() ->
	crypto:rand_uniform(0,9007199254740992).


%%
%% **********  UNIT TESTING   *************************
%%

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
%%
%%
%% call_result_test() ->
%%   flush(),
%%   erwa_sess_man:create_table(),
%%   erwa_publications:create_table(),
%%   erwa_realms:init(),
%%   erwa_realms:add(<<"erwa.test">>),
%%   {ok,SessionId} = erwa_sess_man:register_session(<<"erwa.test">>),
%%   CallInfo = #{procedure_id => 123,
%%                caller_id => SessionId,
%%                call_req_id => 124,
%%                call_options => #{},
%%                call_arguments => [1,4],
%%                call_argumentskw => #{},
%%                callee_ids => [SessionId]
%%                },
%%   {ok,Pid} = start(CallInfo),
%%   monitor(process,Pid),
%%
%%   ok = receive
%%          {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_}} -> ok
%%        end,
%%   ok = yield(Pid,#{},[5],undefined,SessionId),
%%   ok = receive
%%          {erwa,{result, 124, #{}, [5], undefined}} -> ok
%%        end,
%%   ok = receive
%%          {'DOWN', _, process, Pid, _ } -> ok
%%        end,
%% 	erwa_sess_man:drop_table(),
%%   ok.
%%
%%
%%
%% call_error_test() ->
%%   flush(),
%%   erwa_sess_man:create_table(),
%%   erwa_publications:create_table(),
%%   erwa_realms:init(),
%%   erwa_realms:add(<<"erwa.test">>),
%%   {ok,SessionId} = erwa_sess_man:register_session(<<"erwa.test">>),
%%   CallInfo = #{procedure_id => 123,
%%                caller_id => SessionId,
%%                call_req_id => 124,
%%                call_options => #{},
%%                call_arguments => [1,4],
%%                call_argumentskw => #{},
%%                callee_ids => [SessionId]
%%                },
%%   {ok,Pid} = start(CallInfo),
%%   monitor(process,Pid),
%%
%%   ok = receive
%%          {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_ }} -> ok
%%        end,
%%   ok = error(Pid,#{one => error},<<"bad.error">>,undefined,undefined,SessionId),
%%   ok = receive
%%          {erwa,{error,call, 124, #{},<<"bad.error">>,undefined,undefined}} -> ok
%%        end,
%%   ok = receive
%%          {'DOWN', _, process, Pid, _ } -> ok
%%        end,
%% 	erwa_sess_man:drop_table(),
%%   ok.
%%
%%
%%
%% cancel_test() ->
%%   flush(),
%%   erwa_sess_man:create_table(),
%%   erwa_publications:create_table(),
%%   erwa_realms:init(),
%%   erwa_realms:add(<<"erwa.test">>),
%%   {ok,SessionId} = erwa_sess_man:register_session(<<"erwa.test">>),
%%   CallInfo = #{procedure_id => 123,
%%                caller_id => SessionId,
%%                call_req_id => 124,
%%                call_options => #{},
%%                call_arguments => [1,4],
%%                call_argumentskw => #{},
%%                callee_ids => [SessionId]
%%                },
%%   {ok,Pid} = start(CallInfo),
%%   monitor(process,Pid),
%%
%%   ok = receive
%%          {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_ }} -> ok
%%        end,
%%   ok = cancel(Pid,#{}),
%%   ok = receive
%%          {erwa,{interrupt,_,#{invocation_pid := Pid}}} -> ok
%%        end,
%%   ok = error(Pid,#{},canceled,undefined,undefined,SessionId),
%%   ok = receive
%%          {erwa,{error,call, 124, #{},canceled,undefined,undefined}} -> ok
%%        end,
%%   ok = receive
%%          {'DOWN', _, process, Pid, _ } -> ok
%%        end,
%% 	erwa_sess_man:drop_table(),
%%   ok.
%%
%% timeout_test() ->
%%   flush(),
%%   erwa_sess_man:create_table(),
%%   erwa_publications:create_table(),
%%   erwa_realms:init(),
%%   erwa_realms:add(<<"erwa.test">>),
%%   {ok,SessionId} = erwa_sess_man:register_session(<<"erwa.test">>),
%%   CallInfo = #{procedure_id => 123,
%%                caller_id => SessionId,
%%                call_req_id => 124,
%%                call_options => #{timeout => 100},
%%                call_arguments => [1,4],
%%                call_argumentskw => #{},
%%                callee_ids => [SessionId]
%%                },
%%   {ok,Pid} = start(CallInfo),
%%   monitor(process,Pid),
%%
%%   ok = receive
%%          {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_ }} -> ok
%%        end,
%%   ok = receive
%%          {erwa,{interrupt,_,#{invocation_pid := Pid}}} -> ok
%%        end,
%%   ok = error(Pid,[{one,error}],canceled,undefined,undefined,SessionId),
%%   ok = receive
%%          {erwa,{error,call,_,_,_,_,_}} -> ok
%%        end,
%%   ok = receive
%%          {'DOWN', _, process, Pid, _ } -> ok
%%        end,
%% 	erwa_sess_man:drop_table(),
%%   ok.
%%
%%
%%
%% failed_init_test() ->
%%   CallInfo1 = #{procedure_id => 123,
%%                 caller_id => 2393874,
%%                 call_req_id => 124,
%%                 call_options => #{},
%%                 call_arguments => [1,4],
%%                 call_argumentskw => #{},
%%                 callee_ids => []
%%                 },
%%   {error,no_callees} = start(CallInfo1),
%%   CallInfo2 = #{procedure_id => 123,
%%                 caller_id => 2393874,
%%                 call_req_id => 124,
%%                 call_options => #{},
%%                 call_arguments => [1,4],
%%                 call_argumentskw => #{},
%%                 callee_ids => [234,345]
%%                 },
%%   {error,multiple_callees} = start(CallInfo2),
%%   CallInfo3 = #{procedure_id => 123,
%%                 %caller_id => 2393874,
%%                 call_req_id => 124,
%%                 call_options => #{},
%%                 call_arguments => [1,4],
%%                 call_argumentskw => #{},
%%                 callee_ids => [2343]
%%                 },
%%   {error,{badmatch,_}} = start(CallInfo3),
%%   ok.
%%
%%
%% garbage_test() ->
%% 	ok = erwa_sess_man:create_table(),
%%   erwa_publications:create_table(),
%%   erwa_realms:init(),
%%   erwa_realms:add(<<"erwa.test">>),
%%   CallInfo = #{procedure_id => 123,
%%                caller_id => 2393874,
%%                call_req_id => 124,
%%                call_options => #{},
%%                call_arguments => [1,4],
%%                call_argumentskw => #{},
%%                callee_ids => [232]
%%                },
%%   {ok,Pid} = start(CallInfo),
%%   ignored = gen_server:call(Pid,some_garbage),
%%   ok = gen_server:cast(Pid,some_garbage),
%%   Pid ! some_garbage,
%% 	ok = erwa_sess_man:drop_table(),
%%   {ok,stopped} = stop(Pid).
%%
%% -endif.
