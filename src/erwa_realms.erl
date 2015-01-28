%%
%% Copyright (c) 2014 Bas Wegh
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

%% API.
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-export([add/1]).
-export([add/2]).
-export([remove/1]).
-export([get_router/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
	ets=undefined,
  autocreate_realm = false
}).



start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(Name :: binary() ) -> ok.
add(Name) ->
  gen_server:call(?MODULE, {start_realm,Name}).

-spec add(Name :: binary(), Middleware :: atom() ) -> ok.
add(Name, Middleware) ->
  gen_server:call(?MODULE, {start_realm,Name,Middleware}).

-spec remove(Name :: binary()) -> {ok,not_running} | {ok, shutting_down}.
remove(Name) ->
  gen_server:call(?MODULE, {stop_realm,Name}).

-spec get_router(Name :: binary()) -> {ok, Router :: pid()} | {error,not_found}.
get_router(Name) ->
  gen_server:call(?MODULE, {get_router,Name}).

stop() ->
  gen_server:call(?MODULE, {stop}).

  %% gen_server.

init([]) ->
  AutoCreate = get_env(erwa,realm_autocreate,false),
  Ets = ets:new(realms,[set]),
	{ok, #state{ets=Ets, autocreate_realm=AutoCreate}}.



handle_call({start_realm,Name}, _From, State) ->
  Middleware = get_env(erwa,router_middleware,erwa_mw_default),
  {ok,_Pid} = create_new_realm(Name,Middleware,State),
  {reply, ok, State};
handle_call({start_realm,Name,Middleware}, _From, State) ->
  {ok,_Pid} = create_new_realm(Name,Middleware,State),
  {reply, ok, State};


handle_call({stop_realm,Name}, _From, #state{ets=Ets}=State) ->
  Reply =
    case ets:lookup(Ets,Name) of
        [] -> {ok,not_running};
        [{Name,Pid}] ->
          ok = erwa_router:shutdown(Pid),
          {ok, shutting_down}
    end,
  {reply, Reply, State};

handle_call({get_router,Name}, _From, #state{ets=Ets,autocreate_realm=AutoCreate}=State) ->
  Result =
    case {ets:lookup(Ets,Name),AutoCreate} of
        {[],true} ->
          Middleware = get_env(erwa,router_middleware,erwa_mw_default),
          create_new_realm(Name,Middleware,State);
        {[],_} -> {error,not_found};
        {[{Name,Pid}],_} -> {ok,Pid}
    end,
  {reply, Result, State};

handle_call({stop}, _From, State) ->
	{stop,normal,{ok,stopped},State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'DOWN',Ref,process,_Pid,_Reason},State) ->
  remove_router(Ref,State),
  {noreply,State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



-spec create_new_realm(Name :: binary(), Middleware :: atom(), State :: #state{}) -> {ok, Pid :: pid()}.
create_new_realm(Name,Middleware,#state{ets=Ets}) ->
  case ets:lookup(Ets,Name) of
    [] ->
      {ok,Pid} = supervisor:start_child(erwa_router_sup,[[{realm,Name},{middleware,Middleware}]]),
      Ref = monitor(process,Pid),
      true = ets:insert_new(Ets,[{Name,Pid},{Ref,Name}]),
      {ok,Pid};
    [{Name,Pid}] ->
      {ok,Pid}
  end.


remove_router(Ref,#state{ets=Ets}) ->
  case ets:lookup(Ets,Ref) of
    [] ->
      ok;
    [{Ref,Name}] ->
      ets:delete(Ets,Ref),
      ets:delete(Ets,Name),
      ok
  end.


get_env(Application, Par, Def) when is_atom(Application), is_atom(Par) ->
  case application:get_env(Application,Par) of
    undefined -> Def;
    {ok, Value} -> Value
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

start_stop_test() ->
  {ok,_Pid} = start(),
  {ok,stopped} = stop().

add_remove_test() ->
  erwa_sup:start_link(),
  Name = <<"com.doesnotexist.wamp">>,
  0 = get_tablesize(),
  {error,not_found} = get_router(Name),
  0 = get_tablesize(),
  {ok,not_running} = remove(Name),
  0 = get_tablesize(),
  ok = add(Name),
  2 = get_tablesize(),
  ok = add(Name),
  2 = get_tablesize(),
  {ok,_Pid} = get_router(Name),
  {ok,shutting_down} = remove(Name),
  ok = ensure_tablesize(0,5000),
  {ok,stopped} = stop().




-endif.
