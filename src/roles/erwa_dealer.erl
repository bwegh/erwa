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

-include("erwa_model.hrl").

%% API
-export([register/4]).
-export([unregister/3]).
-export([unregister_all/2]).

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

-record(state,
{
  ets = none,
  meta_events = enabled,
  broker = unknown
}).


-spec enable_metaevents(#data{}) -> ok.
enable_metaevents(#data{pid = Pid}) ->
  gen_server:call(Pid, enable_metaevents).

-spec disable_metaevents(#data{}) -> ok.
disable_metaevents(#data{pid = Pid}) ->
  gen_server:call(Pid, disable_metaevents).


-spec get_registrations(#data{}) -> map().
get_registrations(#data{pid = Pid}) ->
  gen_server:call(Pid, get_registrations).

-spec get_registration(#data{}, non_neg_integer()) -> map().
get_registration(#data{pid = Pid}, RegistrationId) ->
  gen_server:call(Pid, {get_registration, RegistrationId}).


-spec register(ProcedureUri :: binary(), Options :: map(), SessionId :: non_neg_integer(), #data{}) -> {ok, RegistrationId :: non_neg_integer()}.
register(ProcedureUri, Options, SessionId, #data{pid = Pid}) ->
  gen_server:call(Pid, {register, ProcedureUri, Options, SessionId}).

-spec unregister(RegistrationId :: non_neg_integer(), SessionId :: non_neg_integer(), #data{}) -> ok.
unregister(RegistrationId, SessionId, #data{pid = Pid}) ->
  gen_server:call(Pid, {unregister, RegistrationId, SessionId}).


-spec unregister_all(SessionId :: non_neg_integer(), #data{}) -> ok.
unregister_all(SessionId, #data{pid = Pid}) ->
  gen_server:call(Pid, {unregister_all, SessionId}).

-spec call(Uri :: binary(), RequestId :: non_neg_integer(), Options :: map(),
    Arguments :: list(), ArgumentsKw :: map(), Session :: term(), Data :: #data{}) ->
  {ok, pid()} | {error, invocation_failed} | {error, procedure_not_found}.
call(Uri, RequestId, Options, Arguments, ArgumentsKw, SessionId, #data{ets = Ets}) ->
  case ets:lookup(Ets, Uri) of
    [#procedure{uri = Uri, ids = Ids, id = ID}] ->
      CallInfo = #{procedure_id => ID,
        caller_id => SessionId,
        call_req_id => RequestId,
        call_options => Options,
        call_arguments => Arguments,
        call_argumentskw => ArgumentsKw,
        callee_ids => Ids
      },
      case erwa_invocation_sup:start_invocation(CallInfo) of
        {ok, Pid} ->
          {ok, Pid};
        _ -> {error, invocation_failed}
      end;
    [] ->
      {error, procedure_not_found}
  end.

-spec get_features(#data{}) -> term().
get_features(#data{features = F}) ->
  F.

start() ->
  gen_server:start(?MODULE, #{broker => unknown}, []).

start(Args) ->
  gen_server:start(?MODULE, Args, []).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

-spec get_data(pid()) -> {ok, #data{}}.
get_data(Pid) ->
  gen_server:call(Pid, get_data).

stop(#data{pid = Pid}) ->
  stop(Pid);
stop(Pid) ->
  gen_server:call(Pid, stop).

%% gen_server.

init(Args) ->
  Ets = ets:new(rpc, [set, {keypos, 2}]),
  #{broker := Broker} = Args,
  {ok, #state{ets = Ets, broker = Broker}}.

handle_call({register, ProcedureUri, Options, SessionId}, _From, State) ->
  Result = register_procedure(ProcedureUri, Options, SessionId, State),
  {reply, Result, State};
handle_call({unregister, RegistrationId, SessionId}, _From, State) ->
  Result = unregister_procedure(RegistrationId, SessionId, State),
  {reply, Result, State};
handle_call({unregister_all, SessionId}, _From, State) ->
  Result = unregister_all_for(SessionId, State),
  {reply, Result, State};
handle_call(get_registrations, _From, #state{ets = Ets} = State) ->
  ExactUris = ets:match(Ets, #procedure{match = exact, id = '$1', uri = '$2', _ = '_'}),
  Filter = fun([Id, Uri], List) ->
    case binary:part(Uri, {0, 5}) == <<"wamp.">> of
      true ->
        List;
      false ->
        [Id | List]
    end
  end,
  Exact = lists:foldl(Filter, [], ExactUris),
  Prefix = lists:flatten(ets:match(Ets, #procedure{match = prefix, id = '$1', _ = '_'})),
  Wildcard = lists:flatten(ets:match(Ets, #procedure{match = wildcard, id = '$1', _ = '_'})),
  {reply, {ok, #{exact => Exact, prefix => Prefix, wildcard => Wildcard}}, State};
handle_call({get_registration, Id}, _From, #state{ets = Ets} = State) ->
  case ets:lookup(Ets, Id) of
    [#id_procedure{id = Id, uri = Uri}] ->
      [#procedure{uri = Uri, invoke = Invoke, id = Id, match = Match, created = Created}] = ets:lookup(Ets, Uri),
      {reply, {ok, #{uri => Uri, invoke => Invoke, id => Id, match => Match, created => Created}}, State};
    [] ->
      {reply, {error, not_found}, State}
  end;
handle_call(get_data, _From, #state{ets = Ets} = State) ->
  {reply, {ok, #data{ets = Ets, pid = self(), features = ?DIALER_FEATURES}}, State};
handle_call(enable_metaevents, _From, State) ->
  {reply, ok, State#state{meta_events = enabled}};
handle_call(disable_metaevents, _From, State) ->
  {reply, ok, State#state{meta_events = disabled}};
handle_call(stop, _From, State) ->
  {stop, normal, {ok, stopped}, State};
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


%% @private
-spec register_procedure(ProcedureUri :: binary(), Options :: map(), SessionId :: non_neg_integer(), State :: #state{}) ->
  {ok, non_neg_integer()} | {error, procedure_already_exists}.
register_procedure(ProcedureUri, Options, SessionId, #state{ets = Ets} = State) ->
  case ets:lookup(Ets, ProcedureUri) of
    [#procedure{uri = ProcedureUri}] ->
      {error, procedure_already_exists};
    [] ->
      {ok, ProcedureId, ProcDetails} = create_procedure(ProcedureUri, Options, SessionId, State),
      ok = add_proc_to_id(ProcedureUri, SessionId, State),
      publish_metaevent(on_create, ProcedureUri, SessionId, ProcDetails, State),
      publish_metaevent(on_register, ProcedureUri, SessionId, ProcedureId, State),
      {ok, ProcedureId}
  end.

%% @private
-spec create_procedure(Url :: binary(), Options :: map(), SessionId :: non_neg_integer(), State :: #state{}) -> {ok, non_neg_integer()}.
create_procedure(Uri, Options, SessionId, #state{ets = Ets} = State) ->
  ProcedureId = erwa_support:gen_id(),
  Invoke = maps:get(invoke, Options, single),
  Match = maps:get(match, Options, exact),
  Created = erlang:universaltime(),
  case ets:insert_new(Ets, [#id_procedure{id = {proc, ProcedureId}, uri = Uri},
    #procedure{uri = Uri, id = ProcedureId, ids = [SessionId], match = Match, invoke = Invoke, options = Options, created = Created}]
  ) of
    true ->
      {ok, ProcedureId, #{id => ProcedureId,
        created => cowboy_clock:rfc1123(Created),
        uri => Uri,
        invoke => Invoke,
        match => Match}};
    _ ->
      create_procedure(Uri, Options, SessionId, State)
  end.

%% @private
unregister_procedure(RegistrationId, SessionId, #state{ets = Ets} = State) when is_integer(RegistrationId) ->
  case ets:lookup(Ets, {proc, RegistrationId}) of
    [] ->
      {error, not_found};
    [#id_procedure{uri = Uri}] ->
      unregister_procedure(Uri, SessionId, State)
  end;
unregister_procedure(Uri, SessionId, #state{ets = Ets} = State) when is_binary(Uri) ->
  [#procedure{uri = Uri, id = ID, ids = Ids} = Proc] = ets:lookup(Ets, Uri),
  case lists:member(SessionId, Ids) of
    false ->
      {error, not_registered};
    true ->
      ok = remove_proc_from_id(Uri, SessionId, State),
      publish_metaevent(on_unregister, Uri, SessionId, ID, State),
      case lists:delete(SessionId, Ids) of
        [] ->
          true = ets:delete(Ets, {proc, ID}),
          true = ets:delete(Ets, Uri),
          publish_metaevent(on_delete, Uri, SessionId, ID, State);
        NewIds ->
          true = ets:insert(Ets, Proc#procedure{ids = NewIds})
      end,
      ok
  end.

%% @private
-spec unregister_all_for(Pid :: pid(), State :: #state{}) -> ok | {error, Reason :: term()}.
unregister_all_for(SessionId, #state{ets = Ets} = State) ->
  case ets:lookup(Ets, {sess, SessionId}) of
    [#id_dialer_info{procs = Procs}] ->
      F = fun(Uri, []) ->
        ok = unregister_procedure(Uri, SessionId, State),
        []
      end,
      lists:foldl(F, [], Procs),
      ok;
    [] ->
      ok
  end.

%% @private
add_proc_to_id(Uri, SessionId, #state{ets = Ets}) ->
  case ets:lookup(Ets, {sess, SessionId}) of
    [#id_dialer_info{procs = Procs} = IdInf] ->
      true = ets:insert(Ets, IdInf#id_dialer_info{procs = [Uri | lists:delete(Uri, Procs)]}),
      ok;
    [] ->
      IdInf = #id_dialer_info{id = {sess, SessionId}, procs = [Uri]},
      true = ets:insert_new(Ets, IdInf),
      ok
  end.

%% @private
remove_proc_from_id(Uri, SessionId, #state{ets = Ets}) ->
  [#id_dialer_info{procs = Procs} = IdInf] = ets:lookup(Ets, {sess, SessionId}),
  case lists:delete(Uri, Procs) of
    [] ->
      true = ets:delete(Ets, {sess, SessionId});
    NewProcs ->
      true = ets:insert(Ets, IdInf#id_dialer_info{procs = NewProcs})
  end,
  ok.

%% @private
publish_metaevent(_, _, _, _, #state{broker = unknown}) ->  ok;
publish_metaevent(_, _, _, _, #state{meta_events = disabled}) ->  ok;
publish_metaevent(Event, ProcedureUri, SessionId, SecondArg, #state{broker = Broker}) ->
  case binary:part(ProcedureUri, {0, 5}) == <<"wamp.">> of
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
      {ok, _} = erwa_broker:publish(MetaTopic, #{}, [SessionId, SecondArg], undefined, no_session, Broker)
  end,
  ok.