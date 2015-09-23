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
-export([yield/5]).
-export([error/6]).

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

-define(CANCEL_TIMEOUT,20000).

-record(state, {
                procedure_id = unknown,
                call_req_id = unknown,
                caller_id = unknown,
                call_options = [],
                call_arguments = undefined,
                call_argumentskw = undefined,
                progressive = false,

                invocation_id = unknown,
                callee_ids = [],
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

yield(Pid,Options,Arguments,ArgumentsKw,SessionId) ->
  gen_server:cast(Pid,{yield,Options,Arguments,ArgumentsKw,SessionId}).

error(Pid,Details,ErrorUri,Arguments,ArgumentsKw,SessionId) ->
  gen_server:cast(Pid,{error,Details,ErrorUri,Arguments,ArgumentsKw,SessionId}).

init(Args) ->
  case check_and_create_state(Args) of
    {ok, #state{callee_ids=Callees,
                caller_id=CallerId,
                call_options = COptions,
                procedure_id=ProcedureId,
                call_arguments=Arguments,
                call_argumentskw=ArgumentsKw} = State} ->
                  OutOptions = #{invocation_pid => self()},
                  NewOptions = case maps:get(disclose_me,COptions,false) of
                                 true ->
                                   maps:put(caller,CallerId,OutOptions);
                                 _ ->
                                   OutOptions
                               end,
                  send_message_to({invocation,set_request_id,ProcedureId,NewOptions,Arguments,ArgumentsKw},Callees),
                  {ok, State};
    {error,Reason} ->
      {stop,Reason}
  end.


handle_call({cancel,_Options},_From,State) ->
  NewState = do_cancel(State),
  {reply,ok,NewState};
handle_call(stop, _From, State) ->
	{stop,normal,{ok,stopped},State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast({yield,Options,Arguments,ArgumentsKw,CalleeId},
            #state{caller_id=CallerId,call_req_id=RequestId,callee_ids=Callees, progressive=Progressive}=State) ->
  case {maps:get(progress,Options,false),Progressive} of
    {true,true} ->
      send_message_to({result, RequestId, #{progress => true}, Arguments, ArgumentsKw},CallerId),
      {noreply,State};
    {true,false} ->
      send_message_to({error,call, RequestId, #{}, <<"erwa.missbehaving_callee">>},CallerId),
      {stop,normal,State};
    _ ->
      Message = {result, RequestId, #{}, Arguments, ArgumentsKw},
      send_message_to(Message,CallerId),
      case lists:delete(CalleeId,Callees) of
        [] ->
          {stop,normal,State#state{callee_ids=[]}};
        NewCallees ->
          {noreply,State#state{callee_ids=NewCallees}}
      end
  end;
handle_cast({error,_Details,ErrorUri,Arguments,ArgumentsKw,CalleeId},#state{caller_id=CallerId,call_req_id=RequestId, callee_ids=Callees}=State) ->
  send_message_to({error, call, RequestId, #{}, ErrorUri, Arguments, ArgumentsKw},CallerId),
  case lists:delete(CalleeId,Callees) of
    [] ->
      {stop,normal,State#state{callee_ids=[]}};
    NewCallees ->
      {noreply,State#state{callee_ids=NewCallees}}
  end;
handle_cast(_Request, State) ->
	{noreply, State}.


handle_info(automatic_cancel, State) ->
  NewState = do_cancel(State),
  {noreply,NewState};
handle_info(shutdown, State) ->
  {stop,normal,State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


do_cancel(#state{canceled=true}=State) ->
  State;
do_cancel(#state{callee_ids=Callees}=State) ->
  send_message_to({interrupt,set_request_id,#{invocation_pid => self()}},Callees),
  timer:send_after(?CANCEL_TIMEOUT,shutdown),
  State#state{canceled=true}.



check_and_create_state(Args) ->
  try
    #{procedure_id := ProcedureId,
      caller_id :=CallerId,
      call_req_id := RequestId,
      call_options := Options,
      call_arguments := Arguments,
      call_argumentskw := ArgumentsKw,
      callee_ids := Callees
      } = Args,
    Progressive = maps:get(receive_progress,Options,false),

    _CallerExclusion = maps:get(caller_exclusion,Options,true),

    case maps:get(timeout,Options,0) of
      Timeout when Timeout > 0 ->
        timer:send_after(Timeout,automatic_cancel);
      _ ->
        ok
    end,

    State = #state{
                   procedure_id = ProcedureId,
                   call_req_id = RequestId,
                   caller_id = CallerId,
                   call_options = Options,
                   call_arguments = Arguments,
                   call_argumentskw = ArgumentsKw,
                   progressive = Progressive,
                   callee_ids = Callees
                   },


    case length(Callees) of
      0 ->
        {error, no_callees};
      1 ->
        {ok, State};
      _ ->
        %multiple callees
        %{ok, State}
        % error for now
        {error,multiple_callees}
    end
  catch _:Reason ->
    {error,Reason}
  end.


-spec send_message_to(Msg :: term(), Peer :: list() | non_neg_integer()) -> ok.
send_message_to(Msg,SessionId) when is_integer(SessionId) ->
  send_message_to(Msg,[SessionId]);
send_message_to(Msg,Peers) when is_list(Peers) ->
  Send = fun(SessionId,[]) ->
           erwa_sessions:send_message_to(Msg,SessionId),
           []
         end,
  lists:foldl(Send,[],Peers),
  ok.





%%
%% **********  UNIT TESTING   *************************
%%

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
  erwa_sessions:create_table(),
  erwa_publications:create_table(),
  erwa_realms:init(),
  erwa_realms:add(<<"erwa.test">>),
  {ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
               caller_id => SessionId,
               call_req_id => 124,
               call_options => #{},
               call_arguments => [1,4],
               call_argumentskw => #{},
               callee_ids => [SessionId]
               },
  {ok,Pid} = start(CallInfo),
  monitor(process,Pid),

  ok = receive
         {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_}} -> ok
       end,
  ok = yield(Pid,#{},[5],undefined,SessionId),
  ok = receive
         {erwa,{result, 124, #{}, [5], undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _ } -> ok
       end,
	erwa_sessions:drop_table(),
  ok.



call_error_test() ->
  flush(),
  erwa_sessions:create_table(),
  erwa_publications:create_table(),
  erwa_realms:init(),
  erwa_realms:add(<<"erwa.test">>),
  {ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
               caller_id => SessionId,
               call_req_id => 124,
               call_options => #{},
               call_arguments => [1,4],
               call_argumentskw => #{},
               callee_ids => [SessionId]
               },
  {ok,Pid} = start(CallInfo),
  monitor(process,Pid),

  ok = receive
         {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_ }} -> ok
       end,
  ok = error(Pid,#{one => error},<<"bad.error">>,undefined,undefined,SessionId),
  ok = receive
         {erwa,{error,call, 124, #{},<<"bad.error">>,undefined,undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _ } -> ok
       end,
	erwa_sessions:drop_table(),
  ok.



cancel_test() ->
  flush(),
  erwa_sessions:create_table(),
  erwa_publications:create_table(),
  erwa_realms:init(),
  erwa_realms:add(<<"erwa.test">>),
  {ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
               caller_id => SessionId,
               call_req_id => 124,
               call_options => #{},
               call_arguments => [1,4],
               call_argumentskw => #{},
               callee_ids => [SessionId]
               },
  {ok,Pid} = start(CallInfo),
  monitor(process,Pid),

  ok = receive
         {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_ }} -> ok
       end,
  ok = cancel(Pid,#{}),
  ok = receive
         {erwa,{interrupt,_,#{invocation_pid := Pid}}} -> ok
       end,
  ok = error(Pid,#{},canceled,undefined,undefined,SessionId),
  ok = receive
         {erwa,{error,call, 124, #{},canceled,undefined,undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _ } -> ok
       end,
	erwa_sessions:drop_table(),
  ok.

timeout_test() ->
  flush(),
  erwa_sessions:create_table(),
  erwa_publications:create_table(),
  erwa_realms:init(),
  erwa_realms:add(<<"erwa.test">>),
  {ok,SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
               caller_id => SessionId,
               call_req_id => 124,
               call_options => #{timeout => 100},
               call_arguments => [1,4],
               call_argumentskw => #{},
               callee_ids => [SessionId]
               },
  {ok,Pid} = start(CallInfo),
  monitor(process,Pid),

  ok = receive
         {erwa,{invocation,set_request_id,123,#{invocation_pid := Pid},[1,4],_ }} -> ok
       end,
  ok = receive
         {erwa,{interrupt,_,#{invocation_pid := Pid}}} -> ok
       end,
  ok = error(Pid,[{one,error}],canceled,undefined,undefined,SessionId),
  ok = receive
         {erwa,{error,call,_,_,_,_,_}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _ } -> ok
       end,
	erwa_sessions:drop_table(),
  ok.



failed_init_test() ->
  CallInfo1 = #{procedure_id => 123,
                caller_id => 2393874,
                call_req_id => 124,
                call_options => #{},
                call_arguments => [1,4],
                call_argumentskw => #{},
                callee_ids => []
                },
  {error,no_callees} = start(CallInfo1),
  CallInfo2 = #{procedure_id => 123,
                caller_id => 2393874,
                call_req_id => 124,
                call_options => #{},
                call_arguments => [1,4],
                call_argumentskw => #{},
                callee_ids => [234,345]
                },
  {error,multiple_callees} = start(CallInfo2),
  CallInfo3 = #{procedure_id => 123,
                %caller_id => 2393874,
                call_req_id => 124,
                call_options => #{},
                call_arguments => [1,4],
                call_argumentskw => #{},
                callee_ids => [2343]
                },
  {error,{badmatch,_}} = start(CallInfo3),
  ok.


garbage_test() ->
	ok = erwa_sessions:create_table(),
  erwa_publications:create_table(),
  erwa_realms:init(),
  erwa_realms:add(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
               caller_id => 2393874,
               call_req_id => 124,
               call_options => #{},
               call_arguments => [1,4],
               call_argumentskw => #{},
               callee_ids => [232]
               },
  {ok,Pid} = start(CallInfo),
  ignored = gen_server:call(Pid,some_garbage),
  ok = gen_server:cast(Pid,some_garbage),
  Pid ! some_garbage,
	ok = erwa_sessions:drop_table(),
  {ok,stopped} = stop(Pid).

-endif.
