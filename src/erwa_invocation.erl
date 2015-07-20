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

-define(CANCEL_TIMEOUT, 20000).

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

-include_lib("eunit/include/eunit.hrl").

start(Args) ->
  gen_server:start(?MODULE, Args, []).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

%% gen_server.


cancel(Pid, Options) ->
  gen_server:call(Pid, {cancel, Options}).

yield(Pid, Options, Arguments, ArgumentsKw, SessionId) ->
  gen_server:cast(Pid, {yield, Options, Arguments, ArgumentsKw, SessionId}).

error(Pid, Details, ErrorUri, Arguments, ArgumentsKw, SessionId) ->
  gen_server:cast(Pid, {error, Details, ErrorUri, Arguments, ArgumentsKw, SessionId}).

init(Args) ->
  case check_and_create_state(Args) of
    {ok, #state{callee_ids = Callees,
      caller_id = CallerId,
      call_options = COptions,
      procedure_id = ProcedureId,
      call_arguments = Arguments,
      call_argumentskw = ArgumentsKw} = State} ->
      OutOptions = #{invocation_pid => self()},
      NewOptions = case maps:get(disclose_me, COptions, false) of
                     true ->
                       maps:put(caller, CallerId, OutOptions);
                     _ ->
                       OutOptions
                   end,
      send_message_to({invocation, set_request_id, ProcedureId, NewOptions, Arguments, ArgumentsKw}, Callees),
      {ok, State};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call({cancel, _Options}, _From, State) ->
  NewState = do_cancel(State),
  {reply, ok, NewState};
handle_call(stop, _From, State) ->
  {stop, normal, {ok, stopped}, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({yield, Options, Arguments, ArgumentsKw, CalleeId},
    #state{caller_id = CallerId, call_req_id = RequestId, callee_ids = Callees, progressive = Progressive} = State) ->
  case {maps:get(progress, Options, false), Progressive} of
    {true, true} ->
      send_message_to({result, RequestId, #{progress => true}, Arguments, ArgumentsKw}, CallerId),
      {noreply, State};
    {true, false} ->
      send_message_to({error, call, RequestId, #{}, <<"erwa.missbehaving_callee">>}, CallerId),
      {stop, normal, State};
    _ ->
      Message = {result, RequestId, #{}, Arguments, ArgumentsKw},
      send_message_to(Message, CallerId),
      case lists:delete(CalleeId, Callees) of
        [] ->
          {stop, normal, State#state{callee_ids = []}};
        NewCallees ->
          {noreply, State#state{callee_ids = NewCallees}}
      end
  end;
handle_cast({error, _Details, ErrorUri, Arguments, ArgumentsKw, CalleeId},
    #state{caller_id = CallerId, call_req_id = RequestId, callee_ids = Callees} = State) ->
  send_message_to({error, call, RequestId, #{}, ErrorUri, Arguments, ArgumentsKw}, CallerId),
  case lists:delete(CalleeId, Callees) of
    [] ->
      {stop, normal, State#state{callee_ids = []}};
    NewCallees ->
      {noreply, State#state{callee_ids = NewCallees}}
  end;
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(automatic_cancel, State) ->
  NewState = do_cancel(State),
  {noreply, NewState};
handle_info(shutdown, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @private
do_cancel(#state{canceled = true} = State) ->
  State;
do_cancel(#state{callee_ids = Callees} = State) ->
  send_message_to({interrupt, set_request_id, #{invocation_pid => self()}}, Callees),
  timer:send_after(?CANCEL_TIMEOUT, shutdown),
  State#state{canceled = true}.

%% @private
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
    Progressive = maps:get(receive_progress, Options, false),

    _CallerExclusion = maps:get(caller_exclusion, Options, true),

    case maps:get(timeout, Options, 0) of
      Timeout when Timeout > 0 ->
        erlang:send_after(self(), Timeout, automatic_cancel);
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
        {error, multiple_callees}
    end
  catch _:Reason ->
    {error, Reason}
  end.

%% @private
-spec send_message_to(Msg :: term(), Peer :: list() | non_neg_integer()) -> ok.
send_message_to(Msg, SessionId) when is_integer(SessionId) ->
  send_message_to(Msg, [SessionId]);
send_message_to(Msg, Peers) when is_list(Peers) ->
  Send = fun(SessionId, []) ->
    erwa_sessions:send_message_to(Msg, SessionId),
    []
  end,
  lists:foldl(Send, [], Peers),
  ok.

%%
%% **********  UNIT TESTING   *************************
%%

-ifdef(TEST).



-endif.
