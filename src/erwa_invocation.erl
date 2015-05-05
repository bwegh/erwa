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
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% API
-export([cancel/2]).
-export([yield/4]).
-export([error/5]).

-export([start/1]).
-export([start_link/1]).
-export([stop/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
                procedure_id = unknown,
                call_req_id = unknown,
                caller_pid = unknown,
                caller_id = unknown,
                call_options = [],
                call_arguments = undefined,
                call_argumentskw = undefined,

                invocation_id = unknown,
                callee_pids = [],
                results = [],
                canceled = false
                }).


start(Args) ->
  gen_server:start(?MODULE, Args, []).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

  %% gen_server.


cancel(Pid,Options) ->
  gen_server:call(Pid,{cancel,Options}).

yield(Pid,Options,Arguments,ArgumentsKw) ->
  gen_server:cast(Pid,{yield,Options,Arguments,ArgumentsKw,self()}).

error(Pid,Details,ErrorUri,Arguments,ArgumentsKw) ->
  gen_server:cast(Pid,{error,Details,ErrorUri,Arguments,ArgumentsKw,self()}).

init(Args) ->
  case check_and_create_state(Args) of
    {ok, #state{callee_pids=Callees,
                caller_id=CallerId,
                call_options = COptions,
                procedure_id=ProcedureId,
                call_arguments=Arguments,
                call_argumentskw=ArgumentsKw} = State} ->
                  OutOptions = [{invocation_pid,self()}],
                  NewOptions = case lists:keyfind(disclose_me,1,COptions) of
                                 {disclose_me,true} ->
                                   [{caller,CallerId}|OutOptions];
                                 _ ->
                                   OutOptions
                               end,
                  send_message_to({invocation,set_request_id,ProcedureId,NewOptions,Arguments,ArgumentsKw},Callees),
                  {ok, State};
    {error,Reason} ->
      {stop,Reason}
  end.


handle_call({cancel,_Options},_From, #state{canceled=false, callee_pids=Callees}=State) ->
  send_message_to({interrupt,set_request_id,[{invocation_pid,self()}]},Callees),
  {reply,ok,State#state{canceled=true}};
handle_call(stop, _From, State) ->
	{stop,normal,{ok,stopped},State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast({yield,Options,Arguments,ArgumentsKw,CalleePid}, #state{caller_pid=Pid,call_req_id=RequestId,callee_pids=Callees}=State) ->
  case lists:keyfind(progress,1,Options) of
    {progress,true} ->
      send_message_to({result, RequestId, [{progress,true}], Arguments, ArgumentsKw},Pid),
      {noreply,State};
    _ ->
      send_message_to({result, RequestId, [], Arguments, ArgumentsKw},Pid),
      case lists:delete(CalleePid,Callees) of
        [] ->
          {stop,normal,State#state{callee_pids=[]}};
        NewCallees ->
          {noreply,State#state{callee_pids=NewCallees}}
      end
  end;
handle_cast({error,Details,ErrorUri,Arguments,ArgumentsKw,CalleePid},#state{caller_pid=Pid,call_req_id=RequestId, callee_pids=Callees}=State) ->
  send_message_to({error, call, RequestId, Details, ErrorUri, Arguments, ArgumentsKw},Pid),
  case lists:delete(CalleePid,Callees) of
    [] ->
      {stop,normal,State#state{callee_pids=[]}};
    NewCallees ->
      {noreply,State#state{callee_pids=NewCallees}}
  end;
handle_cast(_Request, State) ->
	{noreply, State}.


handle_info(automatic_cancel, #state{callee_pids=Callees} = State) ->
  send_message_to({interrupt,set_request_id,[{invocation_pid,self()}]},Callees),
  {noreply, State#state{canceled=true}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


check_and_create_state(Args) ->
  try
    #{procedure_id := ProcedureId,
      caller_pid := CallerPid,
      caller_id :=CallerId,
      call_req_id := RequestId,
      call_options := Options,
      call_arguments := Arguments,
      call_argumentskw := ArgumentsKw,
      callee_pids := Callees
      } = Args,

    State = #state{
                   procedure_id = ProcedureId,
                   call_req_id = RequestId,
                   caller_pid = CallerPid,
                   caller_id = CallerId,
                   call_options = Options,
                   call_arguments = Arguments,
                   call_argumentskw = ArgumentsKw,
                   callee_pids = Callees
                   },
    %% TODO check the options passed


    case length(Callees) of
      0 ->
        {error, no_callees};
      1 ->
        set_timeout_if_needed(Args),
        {ok, State};
      _ ->
        %multiple callees
        %set_timeout_if_needed(Args),
        %{ok, State}
        % error for now
        {error,multiple_callees}
    end
  catch _:Reason ->
    {error,Reason}
  end.


set_timeout_if_needed(#{timeout := Timeout}) when Timeout > 0 ->
  timer:send_after(Timeout,automatic_cancel);
set_timeout_if_needed(_) ->
  ok.

-spec send_message_to(Msg :: term(), Peer :: list() | pid()) -> ok.
send_message_to(Msg,Pid) when is_pid(Pid) ->
  send_message_to(Msg,[Pid]);
send_message_to(Msg,Peers) when is_list(Peers) ->
  Send = fun(Pid,[]) ->
           Pid ! {erwa,Msg},
           []
         end,
  lists:foldl(Send,[],Peers),
  ok.


-ifdef(TEST).

flush() ->
  receive
    _ ->
      flush()
  after 0 ->
    ok
  end.



call_result_test() ->
  flush(),
  CallInfo = #{procedure_id => 123,
               caller_pid => self(),
               caller_id => 2393874,
               call_req_id => 124,
               call_options => [],
               call_arguments => [1,4],
               call_argumentskw => [{}],
               callee_pids => [self()]
               },
  {ok,Pid} = start(CallInfo),
  monitor(process,Pid),

  ok = receive
         {erwa,{invocation,set_request_id,123,[{invocation_pid,Pid}],[1,4],[{}] }} -> ok
       end,
  ok = yield(Pid,[],[5],undefined),
  ok = receive
         {erwa,{result, 124, [], [5], undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _ } -> ok
       end,
  ok.



call_error_test() ->
  flush(),
  CallInfo = #{procedure_id => 123,
               caller_pid => self(),
               caller_id => 2393874,
               call_req_id => 124,
               call_options => [],
               call_arguments => [1,4],
               call_argumentskw => [{}],
               callee_pids => [self()]
               },
  {ok,Pid} = start(CallInfo),
  monitor(process,Pid),

  ok = receive
         {erwa,{invocation,set_request_id,123,[{invocation_pid,Pid}],[1,4],[{}] }} -> ok
       end,
  ok = error(Pid,[{one,error}],<<"bad.error">>,undefined,undefined),
  ok = receive
         {erwa,{error,call, 124, [{one,error}],<<"bad.error">>,undefined,undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _ } -> ok
       end,
  ok.



failed_init_test() ->
  CallInfo1 = #{procedure_id => 123,
                caller_pid => self(),
                caller_id => 2393874,
                call_req_id => 124,
                call_options => [],
                call_arguments => [1,4],
                call_argumentskw => [{}],
                callee_pids => []
                },
  {error,no_callees} = start(CallInfo1),
  CallInfo2 = #{procedure_id => 123,
                caller_pid => self(),
                caller_id => 2393874,
                call_req_id => 124,
                call_options => [],
                call_arguments => [1,4],
                call_argumentskw => [{}],
                callee_pids => [self(),self()]
                },
  {error,multiple_callees} = start(CallInfo2),
  CallInfo3 = #{procedure_id => 123,
                % caller_pid => self(),
                caller_id => 2393874,
                call_req_id => 124,
                call_options => [],
                call_arguments => [1,4],
                call_argumentskw => [{}],
                callee_pids => [self()]
                },
  {error,{badmatch,_}} = start(CallInfo3),
  ok.


garbage_test() ->
  CallInfo = #{procedure_id => 123,
               caller_pid => self(),
               caller_id => 2393874,
               call_req_id => 124,
               call_options => [],
               call_arguments => [1,4],
               call_argumentskw => [{}],
               callee_pids => [self()]
               },
  {ok,Pid} = start(CallInfo),
  ignored = gen_server:call(Pid,some_garbage),
  ok = gen_server:cast(Pid,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop(Pid).

-endif.
