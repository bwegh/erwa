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

-module(erwa_routing).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% API
-export([start/0]).
-export([start_link/0]).
-export([shutdown/1]).
-export([stop/1]).

-export([connect/1]).
-export([disconnect/1]).

-export([get_broker/1]).
-export([get_dealer/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
                broker = unknown,
                dealer = unknown,

                %api_pid = unknown,
                con_ets=none,
                going_down = false,
                timer_ref = none
                }).

-define(SHUTDOWN_TIMEOUT,30000).

start() ->
  gen_server:start(?MODULE, [], []).

start_link() ->
  gen_server:start_link(?MODULE, [], []).


-spec get_broker( pid() ) -> {ok,term()} | {error,going_down}.
get_broker(Pid) ->
   gen_server:call(Pid, get_broker).

-spec get_dealer( pid() ) -> {ok,term()} | {error,going_down}.
get_dealer( Pid) ->
   gen_server:call(Pid, get_dealer).

-spec connect(pid()) -> ok | {error,going_down}.
connect(Pid) ->
  gen_server:call(Pid, connect ).

-spec disconnect(pid() | none) -> ok | {error,going_down}.
disconnect(none) ->
  ok;
disconnect(Pid) ->
  gen_server:call(Pid, disconnect ).

-spec shutdown(pid()) -> ok | {error,going_down}.
shutdown(Pid) ->
  gen_server:call(Pid, shutdown ).

stop(Pid) ->
  gen_server:call(Pid, stop).

  %% gen_server.

init([]) ->
  Ets = ets:new(connections,[set]),
  {ok,BrokerPid} =erwa_broker:start_link(),
  {ok,Broker} = erwa_broker:get_data(BrokerPid),

  {ok,DealerPid} =erwa_dealer:start_link(),
  {ok,Dealer} = erwa_dealer:get_data(DealerPid),
	{ok, #state{con_ets=Ets, broker=Broker, dealer=Dealer}}.


handle_call(stop, _From, State) ->
  ok = close_routing(State),
	{stop,normal,{ok,stopped},State};
handle_call(disconnect, {Pid, _Ref}, #state{con_ets=Ets,going_down=GoDown} = State) ->
  true = ets:delete(Ets,Pid),
  case { GoDown, ets:info(Ets,size) } of
    {true,0} ->
      close_routing(State),
      {stop,normal,State};
    _ ->
      {reply,ok,State}
  end;
handle_call(_, _, #state{going_down=true} = State) ->
  {reply,{error,going_down},State};
handle_call(connect, {Pid, _Ref}, #state{con_ets=Ets} = State) ->
  true = ets:insert(Ets,{Pid}),
	{reply,ok,State};
handle_call(get_broker, _From, #state{broker=Broker} = State) ->
	{reply,{ok,Broker},State};
handle_call(get_dealer, _From, #state{dealer=Dealer} = State) ->
	{reply,{ok,Dealer},State};
handle_call(shutdown, _From, #state{con_ets=Ets} = State) ->
  case ets:info(Ets,size) of
    0 ->
      {ok,TRef} = timer:send_after(1,timeout_force_close),
      {reply,ok,State#state{going_down=true, timer_ref=TRef}};
    _ ->
      ok = send_all_clients(routing_closing,State),
      {ok,TRef} = timer:send_after(?SHUTDOWN_TIMEOUT,timeout_force_close),
      {reply,ok,State#state{going_down=true, timer_ref=TRef}}
  end;
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Request, State) ->
	{noreply, State}.



%handle_info({'DOWN',_Ref,process,Pid,_Reason},#state{connected=Con}=State) ->
%  NewCon = lists:delete(Pid,Con),
%  {noreply,State#state{connected=NewCon}};


handle_info(timeout_force_close, State) ->
  close_routing(State),
  {stop,normal,State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


close_routing(#state{broker=Broker, dealer=Dealer, timer_ref=TRef}=State) ->
  _ = timer:cancel(TRef),
  send_all_clients(shutdown,State),
  {ok,stopped} = erwa_broker:stop(Broker),
  {ok,stopped} = erwa_dealer:stop(Dealer),
  ok.


send_all_clients(Msg,#state{con_ets=Con}) ->
  ok = ets:foldl(fun(Pid,ok) -> Pid ! {erwa_info,Msg}, ok end, ok, Con).



-ifdef(TEST).

start_stop_test() ->
  {ok,Pid} = start(),
  %{ok,stopped} = stop(Pid).
  error = stop(Pid).

simple_routing_test() ->
  {ok,Pid} = start(),
  ok = connect(Pid),
  ok = disconnect(Pid),
  {ok,stopped} = stop(Pid).

garbage_test() ->
  {ok,Pid} = start(),
  ignored = gen_server:call(Pid,some_garbage),
  ok = gen_server:cast(Pid,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop(Pid).


-endif.
