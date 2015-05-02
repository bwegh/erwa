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


%% @doc This module defines the API for applications to use Erwa either to
%% start a router or connect to another router either local or remote.
-module(erwa).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% API for router
-export([start_realm/1]).
-export([start_realm/2]).
-export([stop_realm/1]).
-export([get_routing_for_realm/1]).



-export([get_version/0]).


%% @doc returns the version string for the application, used as agent description
-spec get_version() -> Version::binary().
get_version() ->
  Ver = case application:get_key(vsn) of
    {ok, V} -> list_to_binary(V);
    _ -> <<"UNKNOWN">>
  end,
  << <<"Erwa-">>/binary, Ver/binary >>.




%% for router


%% @doc Start a router for a realm.
-spec start_realm(Name :: binary() ) -> ok | {error, Reason :: term()}.
start_realm(Name) ->
  ?debugFmt("unit tests in ~p~n",[?MODULE]),
  ok = erwa_realms:add(Name),
  ok.

-spec start_realm(Name :: binary(), Middlewares :: [atom()] ) -> ok | {error, Reason :: term()}.
start_realm(Name,Middlewares) when is_list(Middlewares) ->
  ok = erwa_realms:add(Name,Middlewares),
  ok.


%% @doc Stop the router of a realm.
-spec stop_realm(Name :: binary()) -> {ok,Info :: atom()} | {error, Reason :: atom()}.
stop_realm(Name) ->
  erwa_realms:shutdown(Name).

%% @doc Get the router of a realm.
-spec get_routing_for_realm(Realm :: binary() ) -> {ok, Pid :: pid()} | {error, not_found}.
get_routing_for_realm(Realm) ->
  erwa_realms:get_routing(Realm).



