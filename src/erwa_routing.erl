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
-export([start/1]).
-export([start_link/1]).
-export([shutdown/1]).
-export([stop/1]).

-export([connect/2]).
-export([disconnect/1]).

-export([get_broker/1]).
-export([get_dealer/1]).


-export([get_session_count/1]).
-export([get_session_ids/1]).
%-export([get_session_details/2]).

-export([enable_metaevents/1]).
-export([disable_metaevents/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
                realm_name = unknown,
                broker = unknown,
                dealer = unknown,
                %api_pid = unknown,
                con_ets=none,
                going_down = false,
                timer_ref = none,
                meta_events = enabled
                }).

-record(pid_info, {
                   pid = unknown,
                   id = unknown
                   }).

-define(SHUTDOWN_TIMEOUT,30000).

start(RealmName) ->
  gen_server:start(?MODULE, RealmName, []).

start_link(RealmName) ->
  gen_server:start_link(?MODULE, RealmName, []).


-spec get_broker( pid() ) -> {ok,term()} | {error,going_down}.
get_broker(Pid) ->
   gen_server:call(Pid, get_broker).

-spec get_dealer( pid() ) -> {ok,term()} | {error,going_down}.
get_dealer( Pid) ->
   gen_server:call(Pid, get_dealer).

-spec get_session_count(pid()) -> {ok,non_neg_integer()}.
get_session_count(Pid) ->
  gen_server:call(Pid,get_session_count).

-spec get_session_ids(pid()) -> {ok,[non_neg_integer()]}.
get_session_ids(Pid) ->
  gen_server:call(Pid,get_session_ids).

-spec connect(pid(),Session :: term()) -> ok | {error,going_down}.
connect(Pid,Session) ->
  gen_server:call(Pid, {connect,Session} ).

-spec disconnect(pid() | none) -> ok | {error,going_down}.
disconnect(none) ->
  ok;
disconnect(Pid) ->
  gen_server:call(Pid, disconnect ).


-spec enable_metaevents( pid() ) -> ok.
enable_metaevents( Pid ) ->
  gen_server:call(Pid,enable_metaevents).

-spec disable_metaevents( pid() ) -> ok.
disable_metaevents( Pid ) ->
  gen_server:call(Pid,disable_metaevents).

-spec shutdown(pid()) -> ok | {error,going_down}.
shutdown(Pid) ->
  gen_server:call(Pid, shutdown ).

stop(Pid) ->
  gen_server:call(Pid, stop).

  %% gen_server.

init(RealmName) ->
  Ets = ets:new(connections,[set,{keypos,2},protected]),
  ok =  erwa_broker:init(RealmName),

  {ok,DealerPid} =erwa_dealer:start_link(),
  {ok,Dealer} = erwa_dealer:get_data(DealerPid),

  {ok, _CalleePid} = erwa_callee:start_link(#{ dealer=>Dealer, routing=>self(), realm=>RealmName}),

	{ok, #state{con_ets=Ets, dealer=Dealer, realm_name=RealmName}}.


handle_call(stop, _From, State) ->
  ok = close_routing(State),
	{stop,normal,{ok,stopped},State};
handle_call(disconnect, {Pid, _Ref}, #state{con_ets=Ets,going_down=GoDown,timer_ref=TRef} = State) ->
  case ets:lookup(Ets,Pid) of
    [#pid_info{pid=Pid,id=SessionId}] ->
      publish_metaevent(on_leave,SessionId,State);
    _ ->
      ok
  end,
  true = ets:delete(Ets,Pid),
  NewTRef = case { GoDown, ets:info(Ets,size) } of
              {true,0} ->
                _ = timer:cancel(TRef),
                {ok,TR} = timer:send_after(1,timeout_force_close),
                TR;
              _ -> TRef
            end,
  {reply,ok,State#state{timer_ref=NewTRef}};
handle_call(enable_metaevents, _From, State) ->
  {reply,ok,State#state{meta_events=enabled}};
handle_call(disable_metaevents, _From, State) ->
  {reply,ok,State#state{meta_events=disabled}};
handle_call(_, _, #state{going_down=true} = State) ->
  {reply,{error,going_down},State};
handle_call({connect,Session}, {Pid, _Ref}, #state{con_ets=Ets, realm_name=Realm} = State) ->
  SessionId = erwa_session:get_id(Session),
  case ets:lookup(Ets,Pid) of
    [] ->
      %TODO: more data should be sent, which are really needed and useful:
      % - authid
      % - authmethod
      % - authprovider
      % - authrole
      % - transport
      publish_metaevent(on_join,#{realm => Realm, session => SessionId},State);
    _ -> ok
  end,
  true = ets:insert(Ets,#pid_info{pid=Pid,id=SessionId}),
	{reply,ok,State};
handle_call(get_broker, {Pid,_}, #state{broker=Broker,con_ets=Ets} = State) ->
  case ets:lookup(Ets,Pid) of
    [] ->
      {reply,{error,not_connected},State};
    _ ->
	    {reply,{ok,Broker},State}
  end;
handle_call(get_dealer, {Pid,_}, #state{dealer=Dealer,con_ets=Ets} = State) ->
  case ets:lookup(Ets,Pid) of
    [] ->
      {reply,{error,not_connected},State};
    _ ->
	    {reply,{ok,Dealer},State}
  end;
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
handle_call(get_session_count, _From, #state{con_ets=Ets} = State) ->
  Count = ets:info(Ets,size),
  {reply,{ok,Count},State};
handle_call(get_session_ids, _From, #state{con_ets=Ets} = State) ->
  ExtractId = fun(#pid_info{id=Id},IdList) ->
                [Id|IdList]
              end,
  Ids = ets:foldl(ExtractId,[],Ets),
  {reply,{ok,Ids},State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Request, State) ->
	{noreply, State}.



handle_info(timeout_force_close, State) ->
  close_routing(State),
  {stop,normal,State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


close_routing(#state{ dealer=Dealer, timer_ref=TRef, realm_name=RealmName}=State) ->
  _ = timer:cancel(TRef),
  send_all_clients(shutdown,State),
  ok = erwa_broker:cleanup(RealmName),
  {ok,stopped} = erwa_dealer:stop(Dealer),
  ok.


send_all_clients(Msg,#state{con_ets=Con}) ->
  % should here erwa_session:send_message_to be used?
  % yet routing should never be sending anything to another router ... so not for now
  ok = ets:foldl(fun(#pid_info{pid=Pid},ok) -> Pid ! {erwa,Msg}, ok end, ok, Con).

publish_metaevent(_,_,#state{meta_events=disabled}) ->
  ok;
publish_metaevent(Event,Arg,#state{realm_name=Realm}) ->
  MetaTopic = case Event of
                on_join -> <<"wamp.session.on_join">>;
                on_leave -> <<"wamp.session.on_leave">>
              end,
  {ok,_} = erwa_broker:publish(MetaTopic,#{},[Arg],undefined,no_session,Realm),
  ok.

-ifdef(TEST).
-define(REALM,<<"test_routing">>).

start_stop_test() ->
  erwa_sessions:create_table(),
  {ok,Pid} = start(?REALM),
  {ok,stopped} = stop(Pid),
  erwa_sessions:drop_table().

simple_routing_test() ->
  erwa_sessions:create_table(),
  erwa_publications:create_table(),
  {ok,Pid} = start(?REALM),
  Session = erwa_session:set_id(234,erwa_session:create()),
  ok = connect(Pid,Session),
  {ok,_} = get_dealer(Pid),
  %{ok,_} = get_broker(Pid),
  ok = disconnect(Pid),
  {ok,stopped} = stop(Pid),
  erwa_publications:drop_table(),
  erwa_sessions:drop_table().


forced_connection_test() ->
  erwa_sessions:create_table(),
  {ok,Pid} = start(?REALM),
  %{error,not_connected} = get_broker(Pid),
  {error,not_connected} = get_dealer(Pid),
  {ok,stopped} = stop(Pid),
  erwa_sessions:drop_table().


meta_api_test() ->
  erwa_sessions:create_table(),
  erwa_publications:create_table(),
  {ok,Pid} = start(?REALM),
  Session = erwa_session:set_id(234,erwa_session:create()),
  ok = connect(Pid,Session),
  {ok,1} = get_session_count(Pid),
  {ok,[234]} = get_session_ids(Pid),
  ok = disconnect(Pid),
  {ok,stopped} = stop(Pid),
  erwa_publications:drop_table(),
  erwa_sessions:drop_table().

garbage_test() ->
  erwa_sessions:create_table(),
  {ok,Pid} = start(?REALM),
  ignored = gen_server:call(Pid,some_garbage),
  ok = gen_server:cast(Pid,some_garbage),
  Pid ! some_garbage,
  {ok,stopped} = stop(Pid),
  erwa_sessions:drop_table().


-endif.
