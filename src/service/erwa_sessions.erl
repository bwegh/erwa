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

%% API
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-export([register_session/1]).
-export([unregister_session/0]).
-export([send_message_to/2]).


%% for router to router communication
-export([preregister_session/1]).
-export([update_registration/3]).
-export([delete_preregistration/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
  ets = none
}).

-define(TAB, erwa_sessions_tab).

-spec start() -> {ok, pid()}.
start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send_message_to(Msg :: term(), SessionId :: non_neg_integer()) -> ok | {error, unknown}.
send_message_to(Msg, SessionId) ->
  case ets:lookup(?TAB, SessionId) of
    [{SessionId, unknown, _MonitorRef, _Realm}] ->
      {error, unknown};
    [{SessionId, Pid, _MonitorRef, _Realm}] ->
      Pid ! {erwa, Msg},
      ok;
    [] ->
      {error, unknown}
  end.

-spec register_session(Realm :: binary()) -> {ok, non_neg_integer()}.
register_session(Realm) ->
  gen_server:call(?MODULE, {register_session, Realm}).

-spec preregister_session(Id :: non_neg_integer()) -> true | false.
preregister_session(Id) ->
  gen_server:call(?MODULE, {preregister_session, Id}).

-spec update_registration(Id :: non_neg_integer(), Pid :: pid(), Realm :: binary()) -> ok.
update_registration(Id, Pid, Realm) ->
  gen_server:call(?MODULE, {update_registration, Id, Pid, Realm}).

-spec delete_preregistration(Id :: non_neg_integer()) -> ok.
delete_preregistration(Id) ->
  gen_server:call(?MODULE, {delete_preregistration, Id}).

-spec unregister_session() -> ok.
unregister_session() ->
  gen_server:call(?MODULE, unregister_session).

-spec stop() -> {ok, stopped}.
stop() ->
  gen_server:call(?MODULE, stop).

%% gen_server.

init([]) ->
  Ets = ets:new(?TAB, [set, named_table]),
  {ok, #state{ets = Ets}}.

handle_call({register_session, Realm}, {Pid, _Ref}, #state{ets = Ets} = State) ->
  ID = add_session(Pid, Realm, Ets),
  {reply, {ok, ID}, State};
handle_call({preregister_session, Id}, _, #state{ets = Ets} = State) ->
  {reply, ets:insert_new(Ets, {Id, unknown, none, unknown}), State};
handle_call({update_registration, Id, Pid, Realm}, _, #state{ets = Ets} = State) ->
  ets:insert(Ets, {Id, Pid, none, Realm}),
  {reply, ok, State};
handle_call({delete_preregistration, Id}, _, #state{ets = Ets} = State) ->
  delete_session(Id, Ets),
  {reply, ok, State};
handle_call(unregister_session, {Pid, _Ref}, #state{ets = Ets} = State) ->
  ID = get_id_from_pid(Pid, Ets),
  Result = delete_session(ID, Ets),
  {reply, Result, State};
handle_call(stop, _From, State) ->
  {stop, normal, {ok, stopped}, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{ets = Ets} = State) ->
  ID = get_id_from_pid(Pid, Ets),
  delete_session(ID, Ets),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @private
add_session(Pid, Realm, Ets) ->
  ID = crypto:rand_uniform(0, 9007199254740992),
  case ets:insert_new(Ets, {ID, Pid, none, Realm}) of
    true ->
      MonitorRef = monitor(process, Pid),
      true = ets:insert(Ets, [{ID, Pid, MonitorRef, Realm}, {Pid, ID}]),
      ID;
    false ->
      add_session(Pid, Realm, Ets)
  end.

%% @private
get_id_from_pid(Pid, Ets) ->
  case ets:lookup(Ets, Pid) of
    [{Pid, ID}] ->
      ID;
    _ ->
      not_found
  end.

%% @private
delete_session(not_found, _Ets) -> not_found;
delete_session(ID, Ets) ->
  case ets:lookup(Ets, ID) of
    [{ID, Pid, MonitorRef, _Realm}] ->
      true = case MonitorRef of
               none -> true;
               MonitorRef ->
                 demonitor(MonitorRef)
             end,
      true = ets:delete(Ets, ID),
      true = ets:delete(Ets, Pid),
      ok;
    _ ->
      not_found
  end.