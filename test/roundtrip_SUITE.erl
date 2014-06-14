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

-module(roundtrip_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).


%% Tests.
-export([pubsub/1]).

%% ct.

-define(REALM,<<"internal.roundtrip">>).


all() ->
	[pubsub].


init_per_suite(Config) ->
  {ok,_} = application:ensure_all_started(erwa),
  ok = erwa:start_realm(?REALM),
  Config.

end_per_suite(Config) ->
  {ok,_} = erwa:stop_realm(?REALM),
  Config.


all_passed(_C1,_C2,TimeLeft) when TimeLeft =< 0 ->
  false;
all_passed(C1,C2,TimeLeft) ->
  CS1 = erwa_con:get_client_state(C1),
  CS2 = erwa_con:get_client_state(C2),
  case {roundtrip_client1:test_passed(CS1),
        roundtrip_client2:test_passed(CS2)} of
    {true,true} -> true;
    { _, _} ->
      receive
      after 10 ->
        all_passed(C1,C2,TimeLeft-10)
      end
  end.

pubsub(_) ->
  RpcUrl1 = <<"com.test.sum">>,
  RpcUrl2 = <<"com.test.diff">>,
  EventUrl = <<"com.test.event">>,

  C1 = [{rpc_url,RpcUrl1},{event_url,EventUrl}],
  C2 = [{rpc_url,RpcUrl2},{event_url,EventUrl}],
  {ok,Con1} = erwa:connect(?REALM,roundtrip_client1,C1),
  {ok,Con2} = erwa:connect(?REALM,roundtrip_client2,C2),
  true = all_passed(Con1,Con2,5000),
  ok.


