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

-module(roundtrip_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).


%% Tests.
-export([pubsub_local/1]).
-export([pubsub_tcp/1]).
-export([pubsub_combined/1]).

%% ct.

-define(REALM,<<"internal.roundtrip">>).


all() ->
	[pubsub_local,pubsub_tcp,pubsub_combined].


init_per_suite(Config) ->
  ok = application:set_env(awre,erlbin_number,5),
  ok = application:set_env(erwa,erlbin_number,5),
  {ok,_} = application:ensure_all_started(awre),
  {ok,_} = application:ensure_all_started(erwa),
  {ok,_} = application:ensure_all_started(ranch),
  {ok,_} = application:ensure_all_started(sasl),
  {ok,_} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_in_tcp, []),
  ok = erwa:start_realm(?REALM),
  Config.

end_per_suite(Config) ->
  {ok,_} = erwa:stop_realm(?REALM),
  ok = ranch:stop_listener(erwa_tcp),
  Config.


all_passed(_C1,_C2,TimeLeft) when TimeLeft =< 0 ->
  false;
all_passed(C1,C2,TimeLeft) ->
  CS1 = gen_server:call(C1,{test_passed}),
  CS2 = gen_server:call(C2,{test_passed}),
  case {CS1,CS2} of
    {true,true} -> true;
    { _, _} ->
      receive
      after 10 ->
        all_passed(C1,C2,TimeLeft-10)
      end
  end.

pubsub_local(_) ->
  Realm = <<"realm1">>,
  ok = erwa:start_realm(Realm),
  RpcUrl1 = <<"com.test.sum">>,
  RpcUrl2 = <<"com.test.diff">>,
  EventUrl = <<"com.test.event">>,

  C1 = [{rpc_url,RpcUrl1},{remote_rpc,RpcUrl2},{event_url,EventUrl},{realm,?REALM}],
  C2 = [{rpc_url,RpcUrl2},{remote_rpc,RpcUrl1},{event_url,EventUrl},{realm,?REALM}],

  {ok,Con1} = gen_server:start(roundtrip_client1,C1,[]),
  {ok,Con2} = gen_server:start(roundtrip_client2,C2,[]),
  true = all_passed(Con1,Con2,5000),
  {ok,_} = erwa:stop_realm(Realm),
  ok.


pubsub_tcp(_) ->
  Realm = <<"realm2">>,
  ok = erwa:start_realm(Realm),
  RpcUrl1 = <<"com.test.tcp.sum">>,
  RpcUrl2 = <<"com.test.tcp.diff">>,
  EventUrl = <<"com.test.tcp.event">>,

  C1 = [{rpc_url,RpcUrl1},{remote_rpc,RpcUrl2},{event_url,EventUrl},{realm,?REALM},{tcp,true},{enc,raw_erlbin}],
  C2 = [{rpc_url,RpcUrl2},{remote_rpc,RpcUrl1},{event_url,EventUrl},{realm,?REALM},{tcp,true},{enc,raw_msgpack}],

  {ok,Con1} = gen_server:start(roundtrip_client1,C1,[]),
  {ok,Con2} = gen_server:start(roundtrip_client2,C2,[]),
  true = all_passed(Con1,Con2,5000),
  {ok,_} = erwa:stop_realm(Realm),
  ok.


pubsub_combined(_) ->
  Realm = <<"realm3">>,
  ok = erwa:start_realm(Realm),
  RpcUrl1 = <<"com.test.tcp.sum">>,
  RpcUrl2 = <<"com.test.tcp.diff">>,
  EventUrl = <<"com.test.tcp.event">>,

  C1 = [{rpc_url,RpcUrl1},{remote_rpc,RpcUrl2},{event_url,EventUrl},{realm,?REALM}],
  C2 = [{rpc_url,RpcUrl2},{remote_rpc,RpcUrl1},{event_url,EventUrl},{realm,?REALM},{tcp,true},{enc,raw_msgpack}],

  {ok,Con1} = gen_server:start(roundtrip_client1,C1,[]),
  {ok,Con2} = gen_server:start(roundtrip_client2,C2,[]),
  true = all_passed(Con1,Con2,5000),
  {ok,_} = erwa:stop_realm(Realm),
  ok.



