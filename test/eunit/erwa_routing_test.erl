%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2015 17:15
%%%-------------------------------------------------------------------
-module(erwa_routing_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

-include("erwa_model.hrl").

start_stop_test() ->
  erwa_sessions:start(),
  {ok, Pid} = erwa_routing:start(),
  {ok, stopped} = erwa_routing:stop(Pid),
  erwa_sessions:stop().

simple_routing_test() ->
  erwa_sessions:start(),
  {ok, Pid} = erwa_routing:start(),
  Session = #session{id = 234},
  ok = erwa_routing:connect(Pid, Session),
  {ok, _} = erwa_routing:get_dealer(Pid),
  {ok, _} = erwa_routing:get_broker(Pid),
  ok = erwa_routing:disconnect(Pid),
  {ok, stopped} = erwa_routing:stop(Pid),
  erwa_sessions:stop().


forced_connection_test() ->
  erwa_sessions:start(),
  {ok, Pid} = erwa_routing:start(),
  {error, not_connected} = erwa_routing:get_broker(Pid),
  {error, not_connected} = erwa_routing:get_dealer(Pid),
  {ok, stopped} = erwa_routing:stop(Pid),
  erwa_sessions:stop().


meta_api_test() ->
  erwa_sessions:start(),
  {ok, Pid} = erwa_routing:start(),
  Session = #session{id = 234},
  ok = erwa_routing:connect(Pid, Session),
  {ok, 1} = erwa_routing:get_session_count(Pid),
  {ok, [234]} = erwa_routing:get_session_ids(Pid),
  ok = erwa_routing:disconnect(Pid),
  {ok, stopped} = erwa_routing:stop(Pid),
  erwa_sessions:stop().

garbage_test() ->
  erwa_sessions:start(),
  {ok, Pid} = erwa_routing:start(),
  ignored = gen_server:call(Pid, some_garbage),
  ok = gen_server:cast(Pid, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = erwa_routing:stop(Pid),
  erwa_sessions:stop().