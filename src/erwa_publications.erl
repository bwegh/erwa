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

-module(erwa_publications).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-export([get_pub_id/0]).


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


-spec get_pub_id() -> {ok,non_neg_integer()}.
get_pub_id() ->
  gen_server:call(?MODULE, get_pub_id).


-spec stop() -> {ok,stopped}.
stop() ->
  gen_server:call(?MODULE, stop).

  %% gen_server.

init([]) ->
  Ets = ets:new(publication_ids,[set]),
	{ok, #state{ets=Ets}}.


handle_call(get_pub_id, _From, #state{ets=Ets}=State) ->
  ID = new_pub_id(Ets),
  {reply, {ok,ID} , State};

handle_call(stop, _From, State) ->
	{stop,normal,{ok,stopped},State};

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


new_pub_id(Ets) ->
  ID = crypto:rand_uniform(0,9007199254740992),
  case ets:insert_new(Ets,{ID}) of
    true ->
      ID;
    false ->
      new_pub_id(Ets)
  end.


-ifdef(TEST).

get_tablesize() ->
  Pid = whereis(?MODULE),
  Tables = ets:all(),
  [Table] = lists:filter(fun(T) -> ets:info(T,owner) == Pid end,Tables),
  ets:info(Table,size).


stat_stop_test() ->
  {ok,_} = start(),
  {ok,stopped} = stop().

simple_test() ->
  {ok, _ } = start(),
  0 = get_tablesize(),
  {ok,_} = get_pub_id(),
  1 = get_tablesize(),
  {ok,_} = get_pub_id(),
  2 = get_tablesize(),
  {ok,stopped} = stop().


garbage_test() ->
  {ok, Pid} = start(),
  ignored = gen_server:call(?MODULE,some_garbage),
  ok = gen_server:cast(?MODULE,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop().

-endif.

