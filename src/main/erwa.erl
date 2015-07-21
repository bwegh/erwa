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

%% API for router
-export([start_realm/1, start_realm/2, stop_realm/1, get_routing_for_realm/1]).
-export([get_version/0]).
-export([start_websocket/3, start_websocket/4, start_socket/2]).

%% @doc  Start websocket router implementation
-spec start_websocket(string(), integer(), integer()) -> ok.
start_websocket(Path, Port, Acceptors) ->
	start_websocket(Path, Port, Acceptors, []).

%% @doc  Start websocket router implementation with some user's handlers
-spec start_websocket(string(), integer(), integer(), list()) -> ok.
start_websocket(Path, Port, Acceptors, Handlers) ->
	CompleteHandlers = [{Path, erwa_in_ws, []}] ++ Handlers,
	Dispatch = cowboy_router:compile([{'_', CompleteHandlers}]),
	{ok, _} = cowboy:start_http(http, Acceptors, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
	ok.

%% @doc  Start tcp socket router implementation
-spec start_socket(integer(), integer()) -> ok.
start_socket(Port, Acceptors) ->
	{ok, _} = ranch:start_listener(erwa_tcp, Acceptors, ranch_tcp, [{port, Port}], erwa_in_tcp, []),
	ok.

%% @doc returns the version string for the application, used as agent description
-spec get_version() -> Version :: binary().
get_version() ->
	Ver = case application:get_key(erwa, vsn) of
		      {ok, V} -> list_to_binary(V);
		      _ -> <<"UNKNOWN">>
	      end,
	<<<<"Erwa-">>/binary, Ver/binary>>.


%% for router


%% @doc Start a router for a realm.
-spec start_realm(Name :: binary()) -> ok.
start_realm(Name) ->
	ok = erwa_realms_man:add(Name).

-spec start_realm(Name :: binary(), Middlewares :: [atom()]) -> ok.
start_realm(Name, Middlewares) when is_list(Middlewares) ->
	ok = erwa_realms_man:add(Name, Middlewares).


%% @doc Stop the router of a realm.
-spec stop_realm(Name :: binary()) -> {ok, Info :: atom()} | {error, Reason :: atom()}.
stop_realm(Name) ->
	erwa_realms_man:shutdown(Name).

%% @doc Get the router of a realm.
-spec get_routing_for_realm(Realm :: binary()) -> {ok, Pid :: pid()} | {error, not_found}.
get_routing_for_realm(Realm) ->
	erwa_realms_man:get_routing(Realm).