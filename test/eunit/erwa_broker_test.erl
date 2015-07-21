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
-module(erwa_broker_test).
-author("tihon").

-include("erwa_model.hrl").
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
  {ok, Pid} = erwa_broker:start(),
  {ok, Data} = erwa_broker:get_data(Pid),
  0 = get_tablesize(Data),
  ok = erwa_broker:enable_metaevents(Data),
  {ok, stopped} = erwa_broker:stop(Data).

un_subscribe_test() ->
  {ok, Pid} = erwa_broker:start(),
  {ok, Data} = erwa_broker:get_data(Pid),
  ok = erwa_broker:disable_metaevents(Data),
  SessionId = erwa_support:gen_id(),
  0 = get_tablesize(Data),
  {ok, ID1} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId, Data),
  3 = get_tablesize(Data),
  {ok, ID2} = erwa_broker:subscribe(<<"topic.test2">>, #{}, SessionId, Data),
  5 = get_tablesize(Data),
  ok = erwa_broker:unsubscribe(ID1, SessionId, Data),
  3 = get_tablesize(Data),
  {error, not_found} = erwa_broker:unsubscribe(ID1, SessionId, Data),
  ok = erwa_broker:unsubscribe(ID2, SessionId, Data),
  0 = get_tablesize(Data),
  {error, not_found} = erwa_broker:unsubscribe(ID2, SessionId, Data),
  0 = get_tablesize(Data),
  {ok, stopped} = erwa_broker:stop(Data).

unsubscribe_all_test() ->
  {ok, Pid} = erwa_broker:start(),
  {ok, Data} = erwa_broker:get_data(Pid),
  ok = erwa_broker:disable_metaevents(Data),
  SessionId = erwa_support:gen_id(),
  0 = get_tablesize(Data),
  ok = erwa_broker:unsubscribe_all(SessionId, Data),
  0 = get_tablesize(Data),
  {ok, ID1} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId, Data),
  3 = get_tablesize(Data),
  {ok, ID2} = erwa_broker:subscribe(<<"topic.test2">>, #{}, SessionId, Data),
  5 = get_tablesize(Data),
  ok = erwa_broker:unsubscribe_all(SessionId, Data),
  0 = get_tablesize(Data),
  {error, not_found} = erwa_broker:unsubscribe(ID1, SessionId, Data),
  0 = get_tablesize(Data),
  {error, not_found} = erwa_broker:unsubscribe(ID2, SessionId, Data),
  0 = get_tablesize(Data),
  ok = erwa_broker:unsubscribe_all(SessionId, Data),
  0 = get_tablesize(Data),
  {ok, stopped} = erwa_broker:stop(Data).

multiple_un_subscribe_test() ->
  erwa_sessions:start_link(),
  {ok, Pid} = erwa_broker:start(),
  {ok, Data} = erwa_broker:get_data(Pid),
  ok = erwa_broker:disable_metaevents(Data),
  {ok, SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  0 = get_tablesize(Data),
  {ok, ID1} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId, Data),
  3 = get_tablesize(Data),
  {ok, ID2} = erwa_broker:subscribe(<<"topic.test2">>, #{}, SessionId, Data),
  5 = get_tablesize(Data),
  MyPid = self(),
  F =
    fun() ->
      {ok, S2} = erwa_sessions:register_session(<<"erwa.test">>),
      {ok, ID3} = erwa_broker:subscribe(<<"topic.test1">>, #{}, S2, Data),
      MyPid ! {first_subscription, ID3},
      timer:sleep(200),
      {ok, ID4} = erwa_broker:subscribe(<<"topic.test2">>, #{}, S2, Data),
      MyPid ! {second_subscription, ID4},
      timer:sleep(200),
      ok = erwa_broker:unsubscribe_all(S2, Data),
      MyPid ! done,
      ok
    end,
  spawn(F),
  receive
    {first_subscription, ID1} ->
      ok
  end,
  6 = get_tablesize(Data),
  receive
    {second_subscription, ID2} ->
      ok
  end,
  6 = get_tablesize(Data),
  receive
    done ->
      ok
  end,
  5 = get_tablesize(Data),
  ok = erwa_broker:unsubscribe(ID1, SessionId, Data),
  3 = get_tablesize(Data),
  ok = erwa_broker:unsubscribe_all(SessionId, Data),
  0 = get_tablesize(Data),
  erwa_sessions:stop(),
  {ok, stopped} = erwa_broker:stop(Data).

publish_test() ->
  erwa_sessions:start_link(),
  {ok, _} = erwa_publications:start(),
  {ok, Pid} = erwa_broker:start(),
  {ok, Data} = erwa_broker:get_data(Pid),
  ok = erwa_broker:disable_metaevents(Data),
  {ok, SessionId} = erwa_sessions:register_session(<<"erwa.test">>),
  {ok, ID} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId, Data),
  MyPid = self(),
  F =
    fun() ->
      {ok, S2} = erwa_sessions:register_session(<<"erwa.test">>),
      {ok, ID} = erwa_broker:subscribe(<<"topic.test1">>, #{}, S2, Data),
      MyPid ! subscribed,
      receive
        {erwa, {event, ID, PubId, #{}, undefined, undefined}} ->
          MyPid ! {received, PubId}
      end,
      ok = erwa_broker:unsubscribe_all(S2, Data),
      ok
    end,
  spawn(F),
  receive
    subscribed -> ok
  end,
  {ok, PublicationID1} = erwa_broker:publish(<<"topic.test1">>, #{}, undefined, undefined, SessionId, Data),
  receive
    {received, PublicationID1} -> ok
  end,
  {ok, PublicationID2} = erwa_broker:publish(<<"topic.test1">>, #{exclude_me=>false}, undefined, undefined, SessionId, Data),
  ok = receive
         {erwa, {event, ID, PublicationID2, #{}, undefined, undefined}} ->
           ok
       end,
  erwa_sessions:stop(),
  {ok, stopped} = erwa_broker:stop(Data),
  {ok, stopped} = erwa_publications:stop().

exclude_test() ->
  erwa_sessions:start_link(),
  {ok, _} = erwa_publications:start(),
  {ok, Pid} = erwa_broker:start(),
  {ok, Data} = erwa_broker:get_data(Pid),
  ok = erwa_broker:disable_metaevents(Data),
  {ok, SessionId1} = erwa_sessions:register_session(<<"erwa.test">>),
  {ok, SessionId2} = erwa_sessions:register_session(<<"erwa.test">>),
  {ok, ID} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId1, Data),
  MyPid = self(),
  F = fun() ->
    {ok, ID} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId2, Data),
    MyPid ! subscribed,
    Received = receive
                 {erwa, {event, ID, _, #{}, undefined, undefined}} ->
                   true;
                 got_something ->
                   MyPid ! nothing,
                   false
               end,
    case Received of
      true ->
        receive
          {got_something} ->
            MyPid ! yes_got_it
        end;
      false ->
        ok
    end,
    ok = erwa_broker:unsubscribe_all(SessionId2, Data),
    MyPid ! done,
    ok
  end,
  ClientPid = spawn(F),
  receive
    subscribed -> ok
  end,
  {ok, PubID} = erwa_broker:publish(<<"topic.test1">>,
    #{exclude_me => false, exclude => [SessionId2]}, undefined, undefined, SessionId1, Data),
  ok = receive
         {erwa, {event, ID, PubID, #{}, undefined, undefined}} ->
           ok
       end,
  timer:sleep(100),
  ClientPid ! got_something,
  ok = receive
         nothing -> ok;
         yes_got_it -> wrong
       end,
  erwa_sessions:stop(),
  {ok, stopped} = erwa_broker:stop(Data),
  {ok, stopped} = erwa_publications:stop().

eligible_test() ->
  erwa_sessions:start_link(),
  {ok, _} = erwa_publications:start(),
  {ok, Pid} = erwa_broker:start(),
  {ok, Data} = erwa_broker:get_data(Pid),
  ok = erwa_broker:disable_metaevents(Data),
  {ok, SessionId1} = erwa_sessions:register_session(<<"erwa.test">>),
  {ok, SessionId2} = erwa_sessions:register_session(<<"erwa.test">>),

  {ok, ID} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId1, Data),
  MyPid = self(),
  F =
    fun() ->
      {ok, ID} = erwa_broker:subscribe(<<"topic.test1">>, #{}, SessionId2, Data),
      MyPid ! subscribed,
      Received = receive
                   {erwa, {event, ID, _, [], undefined, undefined}} ->
                     true;
                   got_something ->
                     MyPid ! nothing,
                     false
                 end,
      case Received of
        true ->
          receive
            {got_something} ->
              MyPid ! yes_got_it
          end;
        false ->
          ok
      end,
      ok = erwa_broker:unsubscribe_all(SessionId2, Data),
      MyPid ! done,
      ok
    end,
  ClientPid = spawn(F),
  receive
    subscribed -> ok
  end,
  {ok, PubID} = erwa_broker:publish(<<"topic.test1">>,
    #{exclude_me=>false, eligible=>[SessionId1]}, undefined, undefined, SessionId1, Data),
  ok = receive
         {erwa, {event, ID, PubID, #{}, undefined, undefined}} ->
           ok
       end,
  timer:sleep(100),
  ClientPid ! got_something,
  ok = receive
         nothing -> ok;
         yes_got_it -> wrong
       end,
  erwa_sessions:stop(),
  {ok, stopped} = erwa_broker:stop(Data),
  {ok, stopped} = erwa_publications:stop().


garbage_test() ->
  {ok, Pid} = erwa_broker:start(),
  ignored = gen_server:call(Pid, some_garbage),
  ok = gen_server:cast(Pid, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = erwa_broker:stop(Pid).


%% @private
get_tablesize(#data{ets = Ets}) ->
  ets:info(Ets, size).