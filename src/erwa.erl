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

-module(erwa).

%% API for general purpose
-export([start_realm/1]).
-export([stop_realm/1]).
-export([get_router_for_realm/1]).


%% API for connecting to a router (either local or remote)
-export([start_client/0]).
-export([stop_client/1]).

-export([connect/2]).
-export([connect/5]).

-export([subscribe/3,subscribe/4]).
-export([unsubscribe/2]).
-export([publish/3,publish/4,publish/5]).

-export([register/3,register/4]).
-export([unregister/2]).
-export([call/3,call/4,call/5]).
-export([yield/3,yield/4,yield/5]).

%% for general purpose

-spec start_realm(Name :: binary() ) -> ok.
start_realm(Name) ->
  erwa_realms:add(Name).


-spec stop_realm(Name :: binary()) -> {ok,Info :: atom()} | {error, Reason :: atom()}.
stop_realm(Name) ->
  erwa_realms:remove(Name).

-spec get_router_for_realm(Realm :: binary() ) -> {ok, Pid :: pid()} | {error, not_found}.
get_router_for_realm(Realm) ->
  erwa_realms:get_router(Realm).


%% connecting to a (remote) router

start_client() ->
  supervisor:start_child(erwa_con_sup,[[]]).

stop_client(ConPid) ->
  gen_server:cast(ConPid,shutdown).

connect(ConPid,Realm) ->
  gen_server:call(ConPid,{connect,undefined,undefined,Realm,undefined}).

connect(ConPid,Host,Port,Realm,Encoding) ->
  gen_server:call(ConPid,{connect,Host,Port,Realm,Encoding}).


subscribe(ConPid,Options,Topic) ->
  subscribe(ConPid,Options,Topic,undefined).
subscribe(ConPid,Options,Topic,Mfa) ->
  gen_server:call(ConPid,{subscribe,Options,Topic,Mfa}).

unsubscribe(ConPid,SubscriptionId) ->
  gen_server:call(ConPid,{unsubscribe,SubscriptionId}).

publish(ConPid,Options,Topic) ->
  publish(ConPid,Options,Topic,undefined,undefined).
publish(ConPid,Options,Topic,Arguments)->
  publish(ConPid,Options,Topic,Arguments,undefined).
publish(ConPid,Options,Topic,Arguments,ArgumentsKw) ->
  gen_server:call(ConPid,{publish,Options,Topic,Arguments,ArgumentsKw}).


register(ConPid,Options,Procedure) ->
  register(ConPid,Options,Procedure,undefined).
register(ConPid,Options,Procedure,Mfa) ->
  gen_server:call(ConPid,{register,Options,Procedure,Mfa}).

unregister(ConPid,RegistrationId) ->
  gen_server:call(ConPid,{unregister, RegistrationId}).


call(ConPid,Options,ProcedureUrl) ->
  call(ConPid,Options,ProcedureUrl,undefined,undefined).
call(ConPid,Options,ProcedureUrl,Arguments) ->
  call(ConPid,Options,ProcedureUrl,Arguments,undefined).
call(ConPid,Options,ProcedureUrl,Arguments,ArgumentsKw) ->
  gen_server:call(ConPid,{call,Options,ProcedureUrl,Arguments,ArgumentsKw}).

yield(ConPid,RequestId,Details) ->
  yield(ConPid,RequestId,Details,undefined,undefined).
yield(ConPid,RequestId,Details,Arguments) ->
  yield(ConPid,RequestId,Details,Arguments,undefined).
yield(ConPid,RequestId,Details,Arguments,ArgumentsKw) ->
  gen_server:call(ConPid,{yield,RequestId,Details,Arguments,ArgumentsKw}).

