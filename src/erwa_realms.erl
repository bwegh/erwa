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

%% @private
-module(erwa_realms).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% API.
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-export([add/1]).
-export([add/2]).
-export([shutdown/1]).
-export([kill/1]).
-export([get_routing/1]).
-export([get_middleware_list/1]).
-export([set_autocreate/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
  ets = undefined,
  autocreate_realm = false
}).




-spec add(Name :: binary()) -> ok | {error, Reason :: term()}.
add(Name) ->
  MW_List = application:get_env(erwa, router_middleware, [erwa_mw_default]),
  add(Name, MW_List).

-spec add(Name :: binary(), Middlewares :: [atom()]) -> ok | {error, Reason :: term()}.
add(Name, Middlewares) ->
  gen_server:call(?MODULE, {start_realm, Name, Middlewares}).


-spec kill(Name :: binary()) -> {ok, killing} | {error, Reason :: term()}.
kill(Name) ->
  gen_server:call(?MODULE, {kill_realm, Name}).

-spec shutdown(Name :: binary()) -> {ok, shutting_down} | {error, Reason :: term()}.
shutdown(Name) ->
  gen_server:call(?MODULE, {shutdown_realm, Name}).


-spec get_middleware_list(Name :: binary()) -> {ok, [atom()]} | {error, not_found}.
get_middleware_list(Name) ->
  gen_server:call(?MODULE, {get_middleware_list, Name}).

-spec get_routing(Name :: binary()) -> {ok, Realm :: pid()} | {error, not_found}.
get_routing(Name) ->
  gen_server:call(?MODULE, {get_routing, Name}).

-spec set_autocreate(boolean()) -> ok.
set_autocreate(true) ->
  gen_server:call(?MODULE, enable_autocreate);
set_autocreate(false) ->
  gen_server:call(?MODULE, disable_autocreate).



start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, {stop}).


%% gen_server.

init([]) ->
  AutoCreate = application:get_env(erwa, realm_autocreate, false),
  Ets = ets:new(realms, [set]),
  {ok, #state{ets = Ets, autocreate_realm = AutoCreate}}.

handle_call({start_realm, Name, Middleware}, _From, State) ->
  Result = create_new_realm(Name, Middleware, State),
  {reply, Result, State};
handle_call({kill_realm, Name}, _From, State) ->
  Reply = stop_realm({name, Name}, kill, State),
  {reply, Reply, State};
handle_call({shutdown_realm, Name}, _From, State) ->
  Reply = stop_realm({name, Name}, shutdown, State),
  {reply, Reply, State};
handle_call({get_routing, Name}, _From, State) ->
  Result = get_realm_data(routing, Name, State),
  {reply, Result, State};
handle_call({get_middleware_list, Name}, _From, State) ->
  Result = get_realm_data(middleware, Name, State),
  {reply, Result, State};
handle_call(enable_autocreate, _From, State) ->
  {reply, ok, State#state{autocreate_realm = true}};
handle_call(disable_autocreate, _From, State) ->
  {reply, ok, State#state{autocreate_realm = false}};
handle_call({stop}, _From, State) ->
  {stop, normal, {ok, stopped}, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
  _ = stop_realm({monitor, Ref}, clean_up, State),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



-spec create_new_realm(Name :: binary(), MW_List :: [atom()], State :: #state{})
      -> ok | {error, Reason :: term()}.
create_new_realm(Name, MW_List, #state{ets = Ets}) ->
  case ets:lookup(Ets, Name) of
    [] ->
      {ok, Pid} = erwa_routing_sup:start_routing(Name),
      Ref = monitor(process, Pid),
      true = ets:insert_new(Ets, [{Name, active, Pid, Ref, MW_List}, {Ref, Name}]),
      ok;
    [{Name, active, _Pid, _Monitor, _MW_List}] ->
      {error, already_exists};
    [{Name, closing, _Pid, _Monitor, _MW_List}] ->
      {error, shutting_down}
  end.

get_realm_data(Tag, Name, #state{ets = Ets, autocreate_realm = AC} = State) ->
  case ets:lookup(Ets, Name) of
    [{Name, active, Pid, _Ref, MW_List}] ->
      case Tag of
        middleware ->
          {ok, MW_List};
        routing ->
          {ok, Pid}
      end;
    [{Name, _, _Pid, _Ref, _MW_List}] ->
      {error, shutting_down};
    [] ->
      case AC of
        true ->
          MW_List = application:get_env(erwa, router_middleware, [erwa_mw_default]),
          ok = create_new_realm(Name, MW_List, State),
          get_realm_data(Tag, Name, State);
        false ->
          {error, not_found}
      end
  end.


stop_realm({name, Name}, Type, #state{ets = Ets}) ->
  case ets:lookup(Ets, Name) of
    [{Name, RealmState, Pid, Ref, MW_List}] ->
      case {Type, RealmState} of
        {kill, _} ->
          ets:delete(Ets, Ref),
          ets:delete(Ets, Name),
          demonitor(Ref, [flush]),
          {ok, stopped} = erwa_routing:stop(Pid),
          {ok, killed};
        {shutdown, closing} ->
          {ok, shutting_down};
        {shutdown, _} ->
          true = ets:insert(Ets, {Name, closing, Pid, Ref, MW_List}),
          ok = erwa_routing:shutdown(Pid),
          {ok, shutting_down}
      end;
    [] -> {error, not_running};
    Res -> {error, Res}
  end;
stop_realm({monitor, Ref}, clean_up, #state{ets = Ets}) ->
  case ets:lookup(Ets, Ref) of
    [] ->
      ok;
    [{Ref, Name}] ->
      ets:delete(Ets, Ref),
      ets:delete(Ets, Name),
      ok
  end.