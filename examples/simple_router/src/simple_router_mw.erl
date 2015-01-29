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


-module(simple_router_mw).
-behaviour(erwa_middleware).

-export([perm_connect/3]).
-export([authenticate/3]).
-export([perm_publish/5]).
-export([perm_subscribe/3]).
-export([perm_call/5]).
-export([perm_register/3]).


-define(CHALLENGE,<<"{\"nonce\": \"LHRTC9zeOIrt_9U3\", \"authprovider\": \"userdb\", \"authid\": \"peter\",\"timestamp\": \"2015-01-29T20:36:25.448Z\", \"authrole\": \"user\",\"authmethod\": \"wampcra\", \"session\": 3251278072152162}">>).

perm_connect(SessionId, Realm, Details) ->
  io:format("-MW-> ~p:~p~n",[Realm,Details]),
  AuthId = proplists:get_value(authid,Details,undefined),
  case {Realm, AuthId} of
    {<<"realm2">>,<<"peter">>} ->
      {needs_auth, wampcra, [{challenge,?CHALLENGE}]};
    _ ->
      true
  end.

authenticate(_SessionId, Signature, _Extra) ->
  io:format("-MW-> signature~n   ~p~n   ~p~n",[Signature,erwa_auth:wamp_cra(<<"secret1">>,?CHALLENGE)]),
  case Signature == erwa_auth:wamp_cra(<<"secret1">>,?CHALLENGE) of
    true ->
      {true,[{authid,<<"peter">>}]};
    false ->
      false
  end.

perm_publish(_SessionId, _Options, _Topic, _Arguments, _ArgumentsKw) ->
  true.

perm_subscribe(_SessionId, _Options, _Topic) ->
  true.

perm_call(_SessionId, _Options, _Procedure, _Arguments, _ArgumentsKw) ->
  true.

perm_register(_SessionId, _Options, _Procedure) ->
  true.
