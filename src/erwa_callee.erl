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

-module(erwa_callee).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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


-record(state,{
			   realm = unknown,
               sess_id = unknown,
               mapping = #{}
               }).

-define(PROCEDURES,[
					{<<"wamp.subscription.list">>,fun subscription_list/4},
					%{<<"wamp.subscription.match">>,fun subscription_match/4},
					{<<"wamp.subscription.lookup">>,fun subscription_lookup/4},
					{<<"wamp.registration.list">>,fun registration_list/4},
					%{<<"wamp.registration.match">>,fun registration_match/4},
					{<<"wamp.registration.lookup">>,fun registration_lookup/4},
					{<<"wamp.session.count">>,fun session_count/4},
					{<<"wamp.session.list">>,fun session_list/4}
					%{<<"wamp.session.get">>,fun session_get/4}
				   ]).

start(Args) ->
  gen_server:start(?MODULE, Args, []).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
  gen_server:call(Pid, stop).


init(Args) ->
  #{realm:=Realm} = Args,
  {ok,SessionId} = erwa_sess_man:register_session(Realm),
  F = fun({Method,Fun},Map) ->
        {ok,RegId} = erwa_dealer:register(Method, #{match => exact, invoke => single}, SessionId, Realm),
        maps:put(RegId,Fun,Map)
      end,
  Mapping = lists:foldl(F,#{},?PROCEDURES),
  {ok,#state{sess_id = SessionId, mapping=Mapping, realm=Realm}}.


handle_call(_Msg,_From,State) ->
  {reply,ignored,State}.

handle_cast(_Msg,State) ->
  {noreply,State}.

handle_info({erwa,{invocation,_,ProcedureId,Options,Arguments,ArgumentsKw}},#state{sess_id=SessionId,mapping=Mapping}=State) ->
  #{invocation_pid := InvocPid} = Options,
  Fun = maps:get(ProcedureId,Mapping,fun empty_result/4),
  case Fun(Options,Arguments,ArgumentsKw,State) of
    {ok,OutOptions,OutArguments,OutArgumentsKw} ->
      ok = erwa_invocation:yield(InvocPid,OutOptions,OutArguments,OutArgumentsKw,SessionId);
    {error,ErrDetails,ErrorUri,ErrArguments,ErrArgumentsKw} ->
      ok = erwa_invocation:error(InvocPid,ErrDetails,ErrorUri,ErrArguments,ErrArgumentsKw,SessionId)
  end,
  {noreply,State};
handle_info(_Info, State) ->
	{noreply, State}.


session_count(_Options,_Arguments,_ArgumentsKw,#state{realm=Realm}) ->
  {ok,Count} = erwa_sess_man:get_session_count(Realm),
  {ok,#{},[Count],undefined}.

session_list(_Options,_Arguments,_ArgumentsKw,#state{realm=Realm}) ->
  {ok,Ids} = erwa_sess_man:get_session_ids(Realm),
  {ok,#{},Ids,undefined}.

subscription_list(_Options,_Arguments,_ArgumentsKw,#state{realm=Realm}) ->
  {ok,List} = erwa_broker:get_subscriptions(Realm),
  {ok,#{},[List],undefined}.

subscription_lookup(_Options,[SubscriptionId],_ArgumentsKw,#state{realm=Realm}) ->
	case erwa_broker:get_subscription_details(SubscriptionId,Realm) of
		{ok,Details} ->
			{ok, #{},[Details],undefined};
		{error,not_found} ->
			{error,#{},invalid_argument,undefined,undefined}
	end.

registration_list(_Options,_Arguments,_ArgumentsKw,#state{realm=Realm}) ->
  {ok,List} = erwa_dealer:get_registrations(Realm),
  {ok,#{},[List],undefined}.

registration_lookup(_Options,[RegistrationId],_ArgumentsKw,#state{realm=Realm}) ->
  case erwa_dealer:get_registration(RegistrationId, Realm) of
    {ok,Details} ->
      {ok, #{},[Details],undefined};
    {error,not_found} ->
      {error,#{},invalid_argument,undefined,undefined}
  end.

empty_result(_,_,_,_) ->
  {#{},undefined,undefined}.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
