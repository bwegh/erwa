%%
%% Copyright (c) 2014-2015 Bas Wegh
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
-module(erwa_invocation_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

call_result_test() ->
  erwa_test_utils:flush(),
  erwa_sessions:start_link(),
  {ok, SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
    caller_id => SessionId,
    call_req_id => 124,
    call_options => #{},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => [SessionId]
  },
  {ok, Pid} = erwa_invocation:start(CallInfo),
  monitor(process, Pid),

  ok = receive
         {erwa, {invocation, set_request_id, 123, #{invocation_pid := Pid}, [1, 4], _}} -> ok
       end,
  ok = erwa_invocation:yield(Pid, #{}, [5], undefined, SessionId),
  ok = receive
         {erwa, {result, 124, #{}, [5], undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _} -> ok
       end,
  ok.

call_error_test() ->
  erwa_test_utils:flush(),
  erwa_sessions:start_link(),
  {ok, SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
    caller_id => SessionId,
    call_req_id => 124,
    call_options => #{},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => [SessionId]
  },
  {ok, Pid} = erwa_invocation:start(CallInfo),
  monitor(process, Pid),

  ok = receive
         {erwa, {invocation, set_request_id, 123, #{invocation_pid := Pid}, [1, 4], _}} -> ok
       end,
  ok = erwa_invocation:error(Pid, #{one => error}, <<"bad.error">>, undefined, undefined, SessionId),
  ok = receive
         {erwa, {error, call, 124, #{}, <<"bad.error">>, undefined, undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _} -> ok
       end,
  ok.

cancel_test() ->
  erwa_test_utils:flush(),
  erwa_sessions:start_link(),
  {ok, SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
    caller_id => SessionId,
    call_req_id => 124,
    call_options => #{},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => [SessionId]
  },
  {ok, Pid} = erwa_invocation:start(CallInfo),
  monitor(process, Pid),

  ok = receive
         {erwa, {invocation, set_request_id, 123, #{invocation_pid := Pid}, [1, 4], _}} -> ok
       end,
  ok = erwa_invocation:cancel(Pid, #{}),
  ok = receive
         {erwa, {interrupt, _, #{invocation_pid := Pid}}} -> ok
       end,
  ok = erwa_invocation:error(Pid, #{}, canceled, undefined, undefined, SessionId),
  ok = receive
         {erwa, {error, call, 124, #{}, canceled, undefined, undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _} -> ok
       end,
  ok.

timeout_test() ->
  erwa_test_utils:flush(),
  erwa_sessions:start_link(),
  {ok, SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  CallInfo = #{procedure_id => 123,
    caller_id => SessionId,
    call_req_id => 124,
    call_options => #{timeout => 100},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => [SessionId]
  },
  {ok, Pid} = erwa_invocation:start(CallInfo),
  monitor(process, Pid),

  ok = receive
         {erwa, {invocation, set_request_id, 123, #{invocation_pid := Pid}, [1, 4], _}} -> ok
       end,
  ok = receive
         {erwa, {interrupt, _, #{invocation_pid := Pid}}} -> ok
       end,
  ok = erwa_invocation:error(Pid, [{one, error}], canceled, undefined, undefined, SessionId),
  ok = receive
         {erwa, {error, call, _, _, _, _, _}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, Pid, _} -> ok
       end,
  ok.

failed_init_test() ->
  CallInfo1 = #{procedure_id => 123,
    caller_id => 2393874,
    call_req_id => 124,
    call_options => #{},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => []
  },
  {error, no_callees} = erwa_invocation:start(CallInfo1),
  CallInfo2 = #{procedure_id => 123,
    caller_id => 2393874,
    call_req_id => 124,
    call_options => #{},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => [234, 345]
  },
  {error, multiple_callees} = erwa_invocation:start(CallInfo2),
  CallInfo3 = #{procedure_id => 123,
    %caller_id => 2393874,
    call_req_id => 124,
    call_options => #{},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => [2343]
  },
  {error, {badmatch, _}} = erwa_invocation:start(CallInfo3),
  ok.

garbage_test() ->
  CallInfo = #{procedure_id => 123,
    caller_id => 2393874,
    call_req_id => 124,
    call_options => #{},
    call_arguments => [1, 4],
    call_argumentskw => #{},
    callee_ids => [232]
  },
  {ok, Pid} = erwa_invocation:start(CallInfo),
  ignored = gen_server:call(Pid, some_garbage),
  ok = gen_server:cast(Pid, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = erwa_invocation:stop(Pid).