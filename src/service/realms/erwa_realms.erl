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

-module(erwa_realms).
-behaviour(gen_server).

-include("erwa_service.hrl").

%% API.
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).


start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, {stop}).


%% gen_server.
init([]) ->
  ets:new(?REALMS_ETS, [protected, named_table, {read_concurrency, true}, {write_concurrency, true}]),
  {ok, #state{}}.

handle_call({start_realm, Name, Middleware}, _From, State) ->
  Result = create_new_realm(Name, Middleware),
  {reply, Result, State};
handle_call({kill_realm, Name}, _From, State) ->
  Reply = stop_realm({name, Name}, kill),
  {reply, Reply, State};
handle_call({shutdown_realm, Name}, _From, State) ->
  Reply = stop_realm({name, Name}, shutdown),
  {reply, Reply, State};
handle_call(enable_autocreate, _From, State) ->
  application:set_env(erwa, realm_autocreate, true),
  {reply, ok, State};
handle_call(disable_autocreate, _From, State) ->
  application:set_env(erwa, realm_autocreate, false),
  {reply, ok, State};
handle_call({stop}, _From, State) ->
  {stop, normal, {ok, stopped}, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
  stop_realm({monitor, Ref}, clean_up),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


-spec create_new_realm(Name :: binary(), MW_List :: [atom()]) -> ok | {error, Reason :: term()}.
create_new_realm(Name, MW_List) ->
  case ets:lookup(?REALMS_ETS, Name) of
    [] ->
      {ok, Pid} = erwa_routing_sup:start_routing(Name),
      Ref = monitor(process, Pid),
      true = ets:insert_new(?REALMS_ETS, [{Name, active, Pid, Ref, MW_List}, {Ref, Name}]),
      ok;
    [{Name, active, _Pid, _Monitor, _MW_List}] ->
      {error, already_exists};
    [{Name, closing, _Pid, _Monitor, _MW_List}] ->
      {error, shutting_down}
  end.

stop_realm({name, Name}, Type) ->
  case ets:lookup(?REALMS_ETS, Name) of
    [{Name, RealmState, Pid, Ref, MW_List}] ->
      case {Type, RealmState} of
        {kill, _} ->
          ets:delete(?REALMS_ETS, Ref),
          ets:delete(?REALMS_ETS, Name),
          demonitor(Ref, [flush]),
          {ok, stopped} = erwa_routing:stop(Pid),
          {ok, killed};
        {shutdown, closing} ->
          {ok, shutting_down};
        {shutdown, _} ->
          true = ets:insert(?REALMS_ETS, {Name, closing, Pid, Ref, MW_List}),
          ok = erwa_routing:shutdown(Pid),
          {ok, shutting_down}
      end;
    [] -> {error, not_running};
    Res -> {error, Res}
  end;
stop_realm({monitor, Ref}, clean_up) -> %TODO ets protected
  case ets:lookup(?REALMS_ETS, Ref) of
    [] ->
      ok;
    [{Ref, Name}] ->
      ets:delete(?REALMS_ETS, Ref),
      ets:delete(?REALMS_ETS, Name),
      ok
  end.