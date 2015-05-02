


-module(erwa_tcp_mock).
-behaviour(gen_server).

-export([start/0]).
-export([stop/1]).
-export([send/2]).
-export([setopts/2]).
-export([get_buffer/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).




start() ->
  gen_server:start(?MODULE, [], []).

stop(Pid) ->
  gen_server:call(Pid,stop).


send(Pid,Data) ->
  gen_server:call(Pid,{data,Data}).


get_buffer(Pid) ->
  gen_server:call(Pid,get_buffer).
  %% gen_server.

init([]) ->
	{ok, <<"">>}.

setopts(_Pid,_List) ->
  ok.

handle_call({data,Data}, _From, Buffer) ->
	{reply, ok, <<Buffer/binary, Data/binary>> };
handle_call(get_buffer, _From, Buffer) ->
  {reply, {ok, Buffer}, <<"">>};
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
