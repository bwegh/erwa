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

-module(erwa_sessions).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-export([register_session/0]).
-export([unregister_session/0]).
-export([set_realm/1]).


%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state,{
               ets=none
               }).

-spec start() -> {ok,pid()}.
start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec register_session() -> {ok,non_neg_integer()}.
register_session() ->
  gen_server:call(?MODULE, register_session).

-spec unregister_session() -> ok.
unregister_session() ->
  gen_server:call(?MODULE, unregister_session).

-spec set_realm(Name :: binary()) -> ok.
set_realm(Name) ->
  gen_server:call(?MODULE, {set_realm,Name}).

-spec stop() -> {ok,stopped}.
stop() ->
  gen_server:call(?MODULE, stop).

  %% gen_server.

init([]) ->
  Ets = ets:new(sessions,[set]),
	{ok, #state{ets=Ets}}.


handle_call(register_session, {Pid,_Ref}, #state{ets=Ets}=State) ->
  ID = add_session(Pid,Ets),
  {reply, {ok,ID} , State};

handle_call({set_realm,Name}, {Pid,_Ref} , #state{ets=Ets}=State) ->
  Res = set_realm(Name,Pid,Ets),
  {reply, Res , State};

handle_call(unregister_session, {Pid,_Ref}, #state{ets=Ets}=State) ->
  ID = get_id_from_pid(Pid,Ets),
  Result = delete_session(ID,Ets),
  {reply, Result , State};

handle_call(stop, _From, State) ->
	{stop,normal,{ok,stopped},State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'DOWN',_Ref,process,Pid,_Reason},#state{ets=Ets}=State) ->
  ID = get_id_from_pid(Pid,Ets),
  delete_session(ID,Ets),
  {noreply,State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


add_session(Pid,Ets) ->
  ID = crypto:rand_uniform(0,9007199254740992),
  case ets:insert_new(Ets,{ID,Pid,unknown,no_realm}) of
    true ->
      MonitorRef = monitor(process, Pid),
      true = ets:insert(Ets,[{ID,Pid,MonitorRef,no_realm},{Pid,ID}]),
      ID;
    false ->
      add_session(Pid,Ets)
  end.

set_realm(Name,Pid,Ets) ->
  ID = get_id_from_pid(Pid,Ets),
  case ets:lookup(Ets,ID) of
    [{ID,Pid,Ref,no_realm}] ->
      ets:insert(Ets,{ID,Pid,Ref,Name}),
      ok;
    _ -> error
  end.

get_id_from_pid(Pid,Ets) ->
  case ets:lookup(Ets,Pid) of
    [{Pid,ID}] ->
      ID;
    _ ->
      not_found
  end.

delete_session(not_found,_Ets) ->
  not_found;
delete_session(ID,Ets) ->
  case ets:lookup(Ets,ID) of
    [{ID,Pid,MonitorRef,_Realm}] ->
      true = demonitor(MonitorRef),
      true = ets:delete(Ets,ID),
      true = ets:delete(Ets,Pid),
      ok;
    _ ->
      not_found
  end.

-ifdef(TEST).

get_tablesize() ->
  Pid = whereis(?MODULE),
  Tables = ets:all(),
  [Table] = lists:filter(fun(T) -> ets:info(T,owner) == Pid end,Tables),
  ets:info(Table,size).

ensure_tablesize(_Number,MaxTime) when MaxTime =< 0 ->
  timeout;
ensure_tablesize(Number,MaxTime) ->
  case get_tablesize() of
    Number -> ok;
    _ ->
      receive
      after 10 -> ok
      end,
      NewTime = MaxTime - 10,
      ensure_tablesize(Number,NewTime)
  end.

stat_stop_test() ->
  {ok,_} = start(),
  {ok,stopped} = stop().

simple_test() ->
  {ok, _ } = start(),
  0 = get_tablesize(),
  {ok,_} = register_session(),
  2 = get_tablesize(),
  ok = set_realm(<<"cool_realm">>),
  error = set_realm(<<"another_cool_realm">>),
  2 = get_tablesize(),
  ok = unregister_session(),
  0 = get_tablesize(),
  {ok,stopped} = stop().


die_test() ->
  {ok, _ } = start(),
  0 = get_tablesize(),
  F = fun() ->
        erwa_sessions:register_session(),
        receive
        after 200 -> ok
        end
      end,
  spawn(F),
  ok = ensure_tablesize(2,500),
  ok = ensure_tablesize(0,5000),
  {ok,stopped} = stop().

garbage_test() ->
  {ok, Pid} = start(),
  ignored = gen_server:call(?MODULE,some_garbage),
  ok = gen_server:cast(?MODULE,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop().

-endif.

