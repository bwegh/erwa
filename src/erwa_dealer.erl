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
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



%% API
-export([register/4]).
-export([unregister/2]).
-export([unregister_all/1]).

-export([call/7]).

-export([get_features/1]).

-export([enable_metaevents/1]).
-export([disable_metaevents/1]).

-export([get_registrations/1]).
-export([get_registration/2]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% internal
-export([start/0]).
-export([start/1]).
-export([start_link/1]).
-export([stop/1]).

-export([get_data/1]).


-define(FEATURES,#{features => #{
                                 call_canceling =>             true,
                                 call_timeout =>               true,
                                 call_trustlevels =>           false,
                                 callee_blackwhite_listing =>  false,
                                 caller_exclusion =>           false,
                                 caller_identification =>      false,
                                 partitioned_rpc =>            false,
                                 pattern_based_registration => false,
                                 progressive_call_results =>   true
                                 }

                   }
        ).

-record(procedure,{
                   uri = none,
                   id = none,
                   created = unknown,
                   match = exact,
                   invoke = single,
                   options = [],
                   pids = []
                   }).


-record(id_procedure,{
                      id = none,
                      uri = none
                      }).

-record(state, {
                ets = none,
                meta_events = enabled,
                broker = unknown
                }).


-record(data, {
               pid = unknown,
               ets = none,
               features = ?FEATURES
               }).

-record(pid_info, {
                     pid = none,
                     id = unknown,
                     procs = []
                     }).



-spec enable_metaevents( record(data) ) -> ok.
enable_metaevents( #data{pid=Pid} ) ->
  gen_server:call(Pid,enable_metaevents).

-spec disable_metaevents(record(data) ) -> ok.
disable_metaevents( #data{pid=Pid} ) ->
  gen_server:call(Pid,disable_metaevents).


-spec get_registrations( record(data) ) -> map().
get_registrations(#data{pid=Pid}) ->
  gen_server:call(Pid,get_registrations).

-spec get_registration( record(data), non_neg_integer() ) -> map().
get_registration(#data{pid=Pid},RegistrationId) ->
  gen_server:call(Pid,{get_registration,RegistrationId}).


-spec register(ProcedureUri :: binary(), Options :: map(), Session::term(), record(data) ) -> {ok,RegistrationId :: non_neg_integer()}.
register(ProcedureUri, Options, Session, #data{pid=Pid}) ->
   gen_server:call(Pid, {register,ProcedureUri,Options,Session}).

-spec unregister(RegistrationId :: non_neg_integer(), record(data) ) -> ok.
unregister( RegistrationId, #data{pid=Pid}) ->
   gen_server:call(Pid, {unregister, RegistrationId}).


-spec unregister_all( record(data) ) -> ok.
unregister_all( #data{pid=Pid}) ->
   gen_server:call(Pid, unregister_all).

-spec call(Uri :: binary(), RequestId :: non_neg_integer(), Options :: map(),
           Arguments :: list(), ArgumentsKw :: map(), Session::term(), Data :: #data{}) ->
  {ok, pid()} | {error,invocation_failed} | {error, procedure_not_found}.
call(Uri, RequestId, Options, Arguments, ArgumentsKw, Session, #data{ets=Ets}) ->
  case ets:lookup(Ets,Uri) of
    [#procedure{uri=Uri,pids=Pids,id=ID}] ->
      CallInfo = #{procedure_id => ID,
                   caller_pid => self(),
                   caller_id => erwa_session:get_id(Session),
                   call_req_id => RequestId,
                   call_options => Options,
                   call_arguments => Arguments,
                   call_argumentskw => ArgumentsKw,
                   callee_pids => Pids
                   },
      case erwa_invocation_sup:start_invocation(CallInfo) of
        {ok,Pid} ->
          {ok,Pid};
        _ -> {error,invocation_failed}
      end;
    [] ->
      {error,procedure_not_found}
  end.

-spec get_features(record(data)) -> term().
get_features(#data{features = F}) ->
  F.


start() ->
  gen_server:start(?MODULE, #{broker => unknown}, []).


start(Args) ->
  gen_server:start(?MODULE, Args, []).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

-spec get_data( pid() ) -> {ok,record(data)}.
get_data( Pid) ->
   gen_server:call(Pid, get_data).


stop(#data{pid=Pid}) ->
  stop(Pid);
stop(Pid) ->
  gen_server:call(Pid, stop).

  %% gen_server.

init(Args) ->
  Ets = ets:new(rpc,[set,{keypos,2}]),
  #{broker := Broker } = Args,
	{ok, #state{ets=Ets, broker=Broker}}.


handle_call({register,ProcedureUri,Options,Session},{Pid,_Ref},State) ->
  Result = register_procedure(ProcedureUri,Options,Pid,Session,State),
  {reply,Result,State};
handle_call({unregister,RegistrationId},{Pid,_Ref},State) ->
  Result = unregister_procedure(RegistrationId,Pid,State),
  {reply,Result,State};
handle_call(unregister_all,{Pid,_Ref},State) ->
  Result = unregister_all_for(Pid,State),
  {reply,Result,State};
handle_call(get_registrations, _From, #state{ets=Ets} = State) ->
  ExactUris = ets:match(Ets,#procedure{match = exact, id='$1', uri='$2', _='_'}),
  Filter = fun([Id,Uri],List ) ->
             case binary:part(Uri,{0,5}) == <<"wamp.">> of
               true ->
                 List;
               false ->
                 [Id | List]
             end
           end,
  Exact = lists:foldl(Filter,[],ExactUris),
  Prefix = lists:flatten(ets:match(Ets,#procedure{match = prefix, id='$1', _='_'})),
  Wildcard = lists:flatten(ets:match(Ets,#procedure{match = wildcard, id='$1', _='_'})),
  {reply,{ok,#{exact => Exact, prefix => Prefix, wildcard => Wildcard}},State};
handle_call({get_registration,Id}, _From, #state{ets=Ets} = State) ->
  case ets:lookup(Ets,Id) of
    [#id_procedure{id=Id,uri=Uri}] ->
      [#procedure{uri=Uri, invoke=Invoke, id=Id, match=Match, created=Created}] = ets:lookup(Ets,Uri),
      {reply,{ok,#{uri => Uri, invoke => Invoke, id => Id, match => Match, created => Created}},State};
    [] ->
      {reply,{error,not_found},State}
  end;
handle_call(get_data, _From, #state{ets=Ets} = State) ->
	{reply,{ok,#data{ets=Ets,pid=self()}},State};
handle_call(enable_metaevents, _From, State) ->
  {reply,ok,State#state{meta_events=enabled}};
handle_call(disable_metaevents, _From, State) ->
  {reply,ok,State#state{meta_events=disabled}};
handle_call(stop, _From, State) ->
	{stop,normal,{ok,stopped},State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



-spec register_procedure(ProcedureUri :: binary(), Options :: map(), Pid :: pid(), Session::term(), State :: #state{}) ->
  {ok,non_neg_integer()} | {error,procedure_already_exists }.
register_procedure(ProcedureUri,Options,Pid,Session,#state{ets=Ets}=State) ->
  SessionId = erwa_session:get_id(Session),
  case ets:lookup(Ets,ProcedureUri) of
    [#procedure{uri=ProcedureUri}] ->
      {error,procedure_already_exists};
    [] ->
      {ok,ProcedureId,ProcDetails} = create_procedure(ProcedureUri,Options,Pid,State),
      ok = add_proc_to_pid(ProcedureUri,Pid,Session,State),
      publish_metaevent(on_create,ProcedureUri,SessionId,ProcDetails,State),
      publish_metaevent(on_register,ProcedureUri,SessionId,ProcedureId,State),
      {ok,ProcedureId}
  end.

-spec create_procedure(Url :: binary(), Options :: map(), Pid :: pid(), State :: #state{} ) -> {ok,non_neg_integer()}.
create_procedure(Uri,Options,Pid,#state{ets=Ets}=State) ->
  ProcedureId = gen_id(),
  Invoke = maps:get(invoke,Options,single),
  Match = maps:get(match,Options,exact),
  Created = erlang:universaltime(),
  case ets:insert_new(Ets,[#id_procedure{id=ProcedureId,uri=Uri},
                           #procedure{uri=Uri,id=ProcedureId,pids=[Pid],match=Match,invoke=Invoke,options=Options,created=Created}]
                      ) of
    true ->
      {ok,ProcedureId,#{id => ProcedureId,
                        created => cowboy_clock:rfc1123(Created),
                        uri => Uri,
                        invoke => Invoke,
                        match => Match}};
    _ ->
      create_procedure(Uri,Options,Pid,State)
  end.


unregister_procedure(RegistrationId,Pid,#state{ets=Ets} = State) when is_integer(RegistrationId) ->
  case ets:lookup(Ets,RegistrationId) of
    [] ->
      {error, not_found};
    [#id_procedure{id=RegistrationId,uri=Uri}] ->
      unregister_procedure(Uri,Pid,State)
  end;
unregister_procedure(Uri,Pid,#state{ets=Ets}=State) when is_binary(Uri) ->
  [#procedure{uri=Uri,id=ID,pids=Pids} = Proc] = ets:lookup(Ets,Uri),
  SessionId = get_id_for_pid(Pid,State),
  case {lists:member(Pid,Pids),lists:delete(Pid,Pids)} of
    {false, _ } ->
      {error, not_registered};
    {true, [] } ->

      ok = remove_proc_from_pid(Uri,Pid,State),
      publish_metaevent(on_unregister,Uri,SessionId,ID,State),
      true = ets:delete(Ets,ID),
      true = ets:delete(Ets,Uri),
      publish_metaevent(on_delete,Uri,SessionId,ID,State),
      ok;
    {true, NewPids} ->
      ok = remove_proc_from_pid(Uri,Pid,State),
      true = ets:insert(Ets,Proc#procedure{pids=NewPids}),
      publish_metaevent(on_unregister,Uri,SessionId,ID,State),
      ok
  end.

-spec unregister_all_for( Pid :: pid(), State :: #state{} ) -> ok | {error, Reason :: term()}.
unregister_all_for(Pid,#state{ets=Ets}=State) ->
  case ets:lookup(Ets,Pid) of
    [#pid_info{procs=Procs,pid=Pid}] ->
      F = fun(Uri,[]) ->
            ok = unregister_procedure(Uri,Pid,State),
            []
          end,
      lists:foldl(F,[],Procs),
      ok;
    [] ->
      ok
  end.


add_proc_to_pid(Uri,Pid,Session,#state{ets=Ets}) ->
  SessionId = erwa_session:get_id(Session),
  case ets:lookup(Ets,Pid) of
    [#pid_info{procs=Procs} = PI] ->
      true = ets:insert(Ets,PI#pid_info{procs=[Uri|lists:delete(Uri,Procs)]}),
      ok;
    [] ->
      PI = #pid_info{pid=Pid,id=SessionId,procs=[Uri]},
      true = ets:insert_new(Ets,PI),
      ok
  end.



remove_proc_from_pid(Uri,Pid,#state{ets=Ets}) ->
  [#pid_info{procs=Procs} = PI] = ets:lookup(Ets,Pid),
  case lists:delete(Uri,Procs) of
    [] ->
      true = ets:delete(Ets,Pid);
    NewProcs ->
      true = ets:insert(Ets,PI#pid_info{procs=NewProcs})
  end,
  ok.

get_id_for_pid(Pid,#state{ets=Ets}) ->
  [#pid_info{id=SessionId}] = ets:lookup(Ets,Pid),
  SessionId.

gen_id() ->
  crypto:rand_uniform(0,9007199254740992).

publish_metaevent(_,_,_,_,#state{broker=unknown}) ->
  ok;
publish_metaevent(_,_,_,_,#state{meta_events=disabled}) ->
  ok;
publish_metaevent(Event,ProcedureUri,SessionId,SecondArg,#state{broker=Broker}) ->
  case binary:part(ProcedureUri,{0,5}) == <<"wamp.">> of
    true ->
      % do not fire metaevents on "wamp.*" uris
      ok;
    false ->
      MetaTopic = case Event of
                on_create -> <<"wamp.registration.on_create">>;
                on_register -> <<"wamp.registration.on_register">>;
                on_unregister -> <<"wamp.registration.on_unregister">>;
                on_delete -> <<"wamp.registration.on_delete">>
              end,
      {ok,_} = erwa_broker:publish(MetaTopic,#{},[SessionId,SecondArg],undefined,no_session,Broker)
  end,
  ok.


%*************************************************************************************************
%********************************          TESTS              ************************************
%*************************************************************************************************

-ifdef(TEST).

flush() ->
  receive
    _ ->
      flush()
  after 0 ->
    ok
  end.

get_tablesize(#data{ets=Ets}) ->
  ets:info(Ets,size).

ensure_tablesize(_Number,_Data,MaxTime) when MaxTime =< 0 ->
  timeout;
ensure_tablesize(Number,Data,MaxTime) ->
  case get_tablesize(Data) of
    Number -> ok;
    _ ->
      receive
      after 10 -> ok
      end,
      NewTime = MaxTime - 10,
      ensure_tablesize(Number,Data,NewTime)
  end.

start_stop_test() ->
  {ok,Pid} = start(),
  {ok,stopped} = stop(Pid).

set_metaevents_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  enable_metaevents(Data),
  disable_metaevents(Data),
  {ok,stopped} = stop(Data).

un_register_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  Session = erwa_session:create(),
  0 = get_tablesize(Data),
  {ok,ID1} = register(<<"proc.test1">>,#{},Session,Data),
  3 = get_tablesize(Data),
  {ok,ID2} = register(<<"proc.test2">>,#{},Session,Data),
  5 = get_tablesize(Data),
  ok = unregister(ID1,Data),
  3 = get_tablesize(Data),
  {error,not_found} = unregister(ID1,Data),
  ok = unregister(ID2,Data),
  0 = get_tablesize(Data),
  {error,not_found} = unregister(ID2,Data),
  0 = get_tablesize(Data),
  {ok,stopped} = stop(Data).

unregister_all_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  Session = erwa_session:create(),
  0 = get_tablesize(Data),
  ok = unregister_all(Data),
  0 = get_tablesize(Data),
  {ok,ID1} = register(<<"proc.test1">>,#{},Session,Data),
  3 = get_tablesize(Data),
  {ok,ID2} = register(<<"proc.test2">>,#{},Session,Data),
  5 = get_tablesize(Data),
  ok = unregister_all(Data),
  0 = get_tablesize(Data),
  {error,not_found} = unregister(ID1,Data),
  0 = get_tablesize(Data),
  {error,not_found} = unregister(ID2,Data),
  0 = get_tablesize(Data),
  ok = unregister_all(Data),
  0 = get_tablesize(Data),
  {ok,stopped} = stop(Data).

multiple_un_register_test() ->
  flush(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  Session = erwa_session:create(),
  0 = get_tablesize(Data),
  {ok,ID1} = register(<<"proc.test1">>,#{},Session,Data),
  % procedure       x 1
  % id_procedure    x 1
  % pid_info        x 1
  3 = get_tablesize(Data),
  {ok,ID2} = register(<<"proc.test2">>,#{},Session,Data),
  % procedure       x 2
  % id_procedure    x 2
  % pid_info        x 1
  5 = get_tablesize(Data),
  MyPid = self(),
  F = fun() ->
        {error,procedure_already_exists} = erwa_dealer:register(<<"proc.test1">>,#{},Session,Data),
        MyPid ! error_received,
        ok = receive
               try_again -> ok
             end,
        {ok,_} = erwa_dealer:register(<<"proc.test1">>,#{},Session,Data),
        MyPid ! second_subscription_passed,
        ok = receive
               clean -> ok
             end,
        ok = erwa_dealer:unregister_all(Data),
        MyPid ! done,
        ok
      end,
  CPid = spawn(F),
  receive
    error_received ->
      ok
  end,
  % procedure       x 2
  % id_procedure    x 2
  % pid_info        x 1
  5 = get_tablesize(Data),
  ok = unregister(ID1,Data),
  % procedure       x 1
  % id_procedure    x 1
  % pid_info        x 1
  3 = get_tablesize(Data),
  CPid ! try_again,
  ok = receive
         second_subscription_passed -> ok
       end,
  % procedure       x 2
  % id_procedure    x 2
  % pid_info        x 2
  6 = get_tablesize(Data),
  CPid ! clean,
  ok = receive
         done -> ok
       end,
  % procedure       x 1
  % id_procedure    x 1
  % pid_info        x 1
  ok = ensure_tablesize(3,Data,1000),
  ok = unregister(ID2,Data),
  % procedure       x 0
  % id_procedure    x 0
  % pid_info        x 0
  0 = get_tablesize(Data),
  ok = unregister_all(Data),
  % procedure       x 0
  % id_procedure    x 0
  % pid_info        x 0
  0 = get_tablesize(Data),
  flush(),
  {ok,stopped} = stop(Data).


call_test() ->
  erwa_invocation_sup:start_link(),
  flush(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  Session = erwa_session:create(),
  MyPid = self(),
  F = fun() ->
        {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},Session,Data),
        MyPid ! subscribed,
        {ok,A,B,InvocationPid} = receive
                                   {erwa,{invocation,set_request_id,ProcId,#{invocation_pid := InvPid},[In1,In2],undefined}} ->
                                     {ok,In1,In2,InvPid}
                                 end,
        ok = erwa_invocation:yield(InvocationPid,#{},[A+B],undefined),
        ok = erwa_dealer:unregister_all(Data),
        ok
      end,
  CPid = spawn(F),
  monitor(process,CPid),
  ok = receive
         subscribed -> ok
       end,
  RequestId = gen_id(),
  A = gen_id(),
  B = gen_id(),
  C = A+B,
  {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{}, [A,B], undefined,Session,Data),
  monitor(process, InvocationPid),
  ok = receive
         {erwa,{result, RequestId, #{}, [C], undefined}} -> ok
       end,

  ok = receive
           {'DOWN',_,process,CPid,normal} ->
             ok
           end,
  ok = receive
         {'DOWN',_,process,InvocationPid,_} ->
           ok
       end,
  flush(),
  ok.


caller_identification_test() ->
  erwa_invocation_sup:start_link(),
  flush(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  Session = erwa_session:set_id(gen_id(),erwa_session:create()),

  MyPid = self(),
  F = fun() ->
        {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},Session,Data),
        MyPid ! subscribed,
        SessionId = erwa_session:get_id(Session),
        {ok,A,B,InOptions} = receive
                                   {erwa,{invocation,set_request_id,ProcId,Opts,[In1,In2],undefined}} ->
                                     {ok,In1,In2,Opts}
                                 end,
        SessionId = maps:get(caller,InOptions),
        InvocationPid = maps:get(invocation_pid,InOptions),
        ok = erwa_invocation:yield(InvocationPid,#{},[A+B],undefined),
        ok = erwa_dealer:unregister_all(Data),
        receive
          after 100 ->
            MyPid ! done
        end,
        ok
      end,
  spawn(F),
  ok = receive
         subscribed -> ok
       end,
  RequestId = gen_id(),
  A = gen_id(),
  B = gen_id(),
  C = A+B,
  {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{disclose_me => true }, [A,B], undefined,Session,Data),
  monitor(process, InvocationPid),
  ok = receive
         {erwa,{result, RequestId, #{}, [C], undefined}} -> ok
       end,
  ok = receive
         {'DOWN',_,process,InvocationPid,_} ->
           ok
       end,
  ok = receive
         done -> ok
       end,
  flush(),
  ok.


call_cancel_test() ->
  erwa_invocation_sup:start_link(),
  flush(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  Session = erwa_session:set_id(gen_id(),erwa_session:create()),

  MyPid = self(),
  F = fun() ->
        {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},Session,Data),
        MyPid ! subscribed,
        {ok,InOptions} = receive
                           {erwa,{invocation,set_request_id,ProcId,Opts,_,_}} ->
                             {ok,Opts}
                         end,
        InvocationPid = maps:get(invocation_pid,InOptions),
        ok = receive
               {erwa,{interrupt,set_request_id,#{invocation_pid := InvocationPid}}} ->
                 ok
             end,
        ok = erwa_invocation:error(InvocationPid,#{},canceled,undefined,undefined),
        ok = erwa_dealer:unregister_all(Data),
        receive
          after 100 ->
            MyPid ! done
        end,
        ok
      end,
  spawn(F),
  ok = receive
         subscribed -> ok
       end,
  RequestId = gen_id(),
  A = gen_id(),
  B = gen_id(),
  {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{}, [A,B], undefined,Session,Data),
  monitor(process, InvocationPid),
  receive
    after 100 ->
      ok
  end,
  erwa_invocation:cancel(InvocationPid,[]),
  ok = receive
         {erwa,{error,call,_,_,_,_,_}} -> ok
       end,
  ok = receive
         {'DOWN',_,process,InvocationPid,_} ->
           ok
       end,
  ok = receive
         done -> ok
       end,
  flush(),
  ok.


call_progressive_test() ->
  erwa_invocation_sup:start_link(),
  flush(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  Session = erwa_session:set_id(gen_id(),erwa_session:create()),

  MyPid = self(),
  F = fun() ->
        {ok,ProcId} = erwa_dealer:register(<<"proc.sum">>,#{},Session,Data),
        MyPid ! subscribed,
        {ok,InOptions} = receive
                           {erwa,{invocation,set_request_id,ProcId,Opts,_,_}} ->
                             {ok,Opts}
                         end,
        InvocationPid = maps:get(invocation_pid,InOptions),
        ok = erwa_invocation:yield(InvocationPid,#{progress => true},[234],undefined),
        receive
          after 50 ->
            ok
        end,
        ok = erwa_invocation:yield(InvocationPid,#{},[567],undefined),
        ok = erwa_dealer:unregister_all(Data),
        ok
      end,
  spawn(F),
  ok = receive
         subscribed -> ok
       end,
  RequestId = gen_id(),
  A = gen_id(),
  B = gen_id(),
  {ok,InvocationPid} = call(<<"proc.sum">>, RequestId, #{receive_progress => true}, [A,B], undefined,Session,Data),
  monitor(process, InvocationPid),
  ok = receive
         {erwa,{result, RequestId, #{progress := true}, [234], undefined}} -> ok
       end,
  ok = receive
         {erwa,{result, RequestId, #{}, [567], undefined}} -> ok
       end,
  ok = receive
         {'DOWN',_,process,InvocationPid,_} ->
           ok
       end,
  flush(),
  ok.

garbage_test() ->
  {ok,Pid} = start(),
  ignored = gen_server:call(Pid,some_garbage),
  ok = gen_server:cast(Pid,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop(Pid).


-endif.
