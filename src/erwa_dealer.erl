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
-export([register/3]).
-export([unregister/2]).
-export([unregister_all/1]).

-export([call/6]).

-export([get_features/1]).

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


-define(FEATURES,{dealer,[{features,[
                                      {call_canceling,false},
                                      {call_timeout,false},
                                      {call_trustlevels,false},
                                      {callee_blackwhite_listing,false},
                                      {caller_exclusion,false},
                                      {caller_identification,false},
                                      {partitioned_rpc,false},
                                      {pattern_based_registration,false},
                                      {progressive_call_results,false}
                                     ]
                           }]
                  }
        ).

-record(procedure,{
                   uri = none,
                   id = none,
                   options = [],
                   pids = []
                   }).


-record(id_procedure,{
                      id = none,
                      uri = none
                      }).

-record(state, {
                ets = none
                }).


-record(data, {
               pid = unknown,
               ets = none,
               features = ?FEATURES
               }).

-record(pid_info, {
                     pid = none,
                     procs = []
                     }).


-spec register(ProcedureUri :: binary(), Options :: list(), record(data) ) -> {ok,RegistrationId :: non_neg_integer()}.
register(ProcedureUri, Options, #data{pid=Pid}) ->
   gen_server:call(Pid, {register,ProcedureUri,Options}).

-spec unregister(RegistrationId :: non_neg_integer(), record(data) ) -> ok.
unregister( RegistrationId, #data{pid=Pid}) ->
   gen_server:call(Pid, {unregister, RegistrationId}).


-spec unregister_all( record(data) ) -> ok.
unregister_all( #data{pid=Pid}) ->
   gen_server:call(Pid, unregister_all).

-spec call(Uri :: binary(), RequestId :: non_neg_integer(), Options :: list(),
           Arguments :: list(), ArgumentsKw :: list(), Data :: #data{}) ->
  ok | {error,invocation_failed} | {error, procedure_not_found}.
call(Uri, RequestId, Options, Arguments, ArgumentsKw, #data{ets=Ets}) ->
  case ets:lookup(Ets,Uri) of
    [#procedure{uri=Uri,pids=Pids,id=ID}] ->
      CallInfo = #{procedure_id => ID,
                   caller_pid => self(),
                   call_req_id => RequestId,
                   call_options => Options,
                   call_arguments => Arguments,
                   call_argumentskw => ArgumentsKw,
                   callee_pids => Pids
                   },
      case erwa_invocation_sup:start_invocation(CallInfo) of
        {ok, _ } ->
          ok;
        _ -> {error,invocation_failed}
      end;
    [] ->
      {error,procedure_not_found}
  end.

-spec get_features(record(data)) -> term().
get_features(#data{features = F}) ->
  F.


start() ->
  gen_server:start(?MODULE, [], []).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec get_data( pid() ) -> {ok,record(data)}.
get_data( Pid) ->
   gen_server:call(Pid, get_data).


stop(#data{pid=Pid}) ->
  stop(Pid);
stop(Pid) ->
  gen_server:call(Pid, stop).

  %% gen_server.

init([]) ->
  Ets = ets:new(rpc,[set,{keypos,2}]),
	{ok, #state{ets=Ets}}.


handle_call({register,ProcedureUri,Options},{Pid,_Ref},State) ->
  Result = register_procedure(ProcedureUri,Options,Pid,State),
  {reply,Result,State};
handle_call({unregister,RegistrationId},{Pid,_Ref},State) ->
  Result = unregister_procedure(RegistrationId,Pid,State),
  {reply,Result,State};
handle_call(unregister_all,{Pid,_Ref},State) ->
  Result = unregister_all_for(Pid,State),
  {reply,Result,State};
handle_call(get_data, _From, #state{ets=Ets} = State) ->
	{reply,{ok,#data{ets=Ets,pid=self()}},State};
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



-spec register_procedure(ProcedureUrl :: binary(), Options :: list(), Pid :: pid(), State :: #state{}) ->
  {ok,non_neg_integer()} | {error,procedure_already_exists }.
register_procedure(ProcedureUrl,Options,Pid,#state{ets=Ets}=State) ->
  case ets:lookup(Ets,ProcedureUrl) of
    [#procedure{uri=ProcedureUrl}] ->
      {error,procedure_already_exists};
    [] ->
      ok = add_proc_to_pid(ProcedureUrl,Pid,State),
      create_procedure(ProcedureUrl,Options,Pid,State)
  end.

-spec create_procedure(Url :: binary(), Options :: list(), Pid :: pid(), State :: #state{} ) -> {ok,non_neg_integer()}.
create_procedure(Uri,Options,Pid,#state{ets=Ets}=State) ->
  ProcedureId = gen_id(),
  case ets:insert_new(Ets,[#id_procedure{id=ProcedureId,uri=Uri},
                           #procedure{uri=Uri,id=ProcedureId,pids=[Pid],options=Options}]
                      ) of
    true ->
      {ok,ProcedureId};
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
  case {lists:member(Pid,Pids),lists:delete(Pid,Pids)} of
    {false, _ } ->
      {error, not_registered};
    {true, [] } ->

      ok = remove_proc_from_pid(Uri,Pid,State),
      true = ets:delete(Ets,ID),
      true = ets:delete(Ets,Uri),
      ok;
    {true, NewPids} ->
      ok = remove_proc_from_pid(Uri,Pid,State),
      true = ets:insert(Ets,Proc#procedure{pids=NewPids}),
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


add_proc_to_pid(Uri,Pid,#state{ets=Ets}) ->
  case ets:lookup(Ets,Pid) of
    [#pid_info{procs=Procs} = PI] ->
      true = ets:insert(Ets,PI#pid_info{procs=[Uri|lists:delete(Uri,Procs)]}),
      ok;
    [] ->
      PI = #pid_info{pid=Pid,procs=[Uri]},
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

gen_id() ->
  crypto:rand_uniform(0,9007199254740992).

-ifdef(TEST).

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

features_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  ?FEATURES = get_features(Data),
  timeout = ensure_tablesize(1,Data,0),
  {ok,stopped} = stop(Data).

un_register_test() ->
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  0 = get_tablesize(Data),
  {ok,ID1} = register(<<"proc.test1">>,[],Data),
  3 = get_tablesize(Data),
  {ok,ID2} = register(<<"proc.test2">>,[],Data),
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
  0 = get_tablesize(Data),
  ok = unregister_all(Data),
  0 = get_tablesize(Data),
  {ok,ID1} = register(<<"proc.test1">>,[],Data),
  3 = get_tablesize(Data),
  {ok,ID2} = register(<<"proc.test2">>,[],Data),
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
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  0 = get_tablesize(Data),
  {ok,ID1} = register(<<"proc.test1">>,[],Data),
  % procedure       x 1
  % id_procedure    x 1
  % pid_info        x 1
  3 = get_tablesize(Data),
  {ok,ID2} = register(<<"proc.test2">>,[],Data),
  % procedure       x 2
  % id_procedure    x 2
  % pid_info        x 1
  5 = get_tablesize(Data),
  MyPid = self(),
  F = fun() ->
        {error,procedure_already_exists} = erwa_dealer:register(<<"proc.test1">>,[],Data),
        MyPid ! error_received,
        ok = receive
               try_again -> ok
             end,
        {ok,_} = erwa_dealer:register(<<"proc.test1">>,[],Data),
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
  {ok,stopped} = stop(Data).


call_test() ->
  erwa_invocation_sup:start_link(),
  {ok,Pid} = start(),
  {ok,Data} = get_data(Pid),
  MyPid = self(),
  F = fun() ->
        {ok,ID} = erwa_dealer:register(<<"proc.sum">>,[],Data),
        MyPid ! subscribed,
        {ok,A,B} = receive
                     {erwa,{invocation,set_request_id,ID,[{invocation_pid,InvocationPid}],[In1,In2],undefined}} ->
                       {ok,In1,In2}
                   end,
        ok = erwa_invocation:yield(InvocationPid,[],[A+B],undefined),
        ok = erwa_dealer:unregister_all(Data),
        receive
          after 100 ->
            MyPid ! done
        end,
        ok
      end,
  CPid = spawn(F),
  ok = receive
         subscribed -> ok
       end,
  RequestId = gen_id(),
  A = gen_id(),
  B = gen_id(),
  C = A+B,
  ok = erwa_dealer:call(<<"proc.sum">>, RequestId, [], [A,B], undefined,Data),
  ok = receive
         {erwa,{result, RequestId, [], [C], undefined}} -> ok
       end,
  ok = receive
         done -> ok
       end,
  ok.


garbage_test() ->
  {ok,Pid} = start(),
  ignored = gen_server:call(Pid,some_garbage),
  ok = gen_server:cast(Pid,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop(Pid).


-endif.
