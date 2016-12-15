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
-export([stop_realm/1]).


-export([get_version/0]).
-export([start_websocket/3]).
-export([start_websocket/4]).
-export([start_websocket/5]).
-export([start_socket/2]).
-export([start_socket/3]).


%% @doc returns the version string for the application, used as agent description
-spec get_version() -> Version::binary().
get_version() ->
  Ver = case application:get_key(vsn) of
    {ok, V} -> list_to_binary(V);
    _ -> <<"UNKNOWN">>
  end,
  << <<"Erwa-">>/binary, Ver/binary >>.

%% @doc start router listening on websocket
-spec start_websocket( Path :: string(), Port :: integer(), Acceptors ::
											 integer() ) -> ok.
start_websocket(Path, Port, Acceptors) -> 
	start_websocket(Path, Port, no_cert, Acceptors, []).



%% @doc start router with ssl websocket
-spec start_websocket( Path :: string(), Port :: integer(),  PathToCertfile ::
                       list(), Acceptors ::
											 integer() ) -> ok.
start_websocket(Path, Port, PathToCertfile, Acceptors) -> 
	start_websocket(Path, Port, PathToCertfile, Acceptors, []).



%% @doc start router listening on websocket with custom handlers added
-spec start_websocket( Path :: string(), Port :: integer(), PathToCertfile ::
                       list(), Acceptors ::
								 non_neg_integer(), Handlers :: [term()] ) -> ok.
start_websocket(Path, Port, no_cert, Acceptors, Handlers) ->
  Dispatch = cowboy_router:compile([{'_',[ {Path, erwa_in_ws, [] } | Handlers] }]),
  
  {ok, CowboyVsn} = application:get_key(cowboy,vsn),
  case string:str(CowboyVsn, "1.") of
	  1 ->
		  %% This is for cowboy-1.x
		  {ok, _} = cowboy:start_http(erwa_http, Acceptors, [{port, Port}], [{env, [{dispatch, Dispatch}]}]);
	  _->
		   %% This is for cowboy-2.x or newer
		  {ok, _} = cowboy:start_clear(erwa_http, Acceptors, [{port,  Port}], #{env => #{dispatch => Dispatch}})
  end,	
  ok;
start_websocket(Path, Port, PathToCertfile, Acceptors, Handlers) ->
	Dispatch = cowboy_router:compile([{'_',[ {Path, erwa_in_ws,[] } | Handlers] }]),
	
	{ok, CowboyVsn} = application:get_key(cowboy,vsn),
	
  	case string:str(CowboyVsn, "1.") of
		1 ->
			%% This is for cowboy-1.x
			{ok, _} = cowboy:start_http(erwa_http, Acceptors, [
													   {port,Port},
													   {certfile,PathToCertfile}], 
								[{env,
								  [{dispatch,Dispatch}]}
								]
							   ),
			ok;
		_->
			%% This is for cowboy-2.x
			{ok, _} = cowboy:start_clear(erwa_http, Acceptors, [{port,  Port},
																{certfile,PathToCertfile}], 
										 #{env => #{dispatch => Dispatch}}),
			ok
	end.


%% @doc start the router listening for raw tcp, ssl, connections
-spec start_socket(Port :: non_neg_integer(), PathToCertfile :: list(), Acceptors :: non_neg_integer()) ->
	ok.
start_socket(Port, PathToCertfile, Acceptors) ->
	{ok, _} = ranch:start_listener(erwa_ssl, Acceptors, ranch_ssl, 
                                 [
                                  {port, Port},
                                  {certfile,PathToCertfile}
                                 ],
                                 erwa_in_tcp, []),
	ok.


%% @doc start the router listening for raw tcp connections
-spec start_socket(Port :: non_neg_integer(), Acceptors :: non_neg_integer()) ->
	ok.
start_socket(Port, Acceptors) ->
	{ok, _} = ranch:start_listener(erwa_tcp, Acceptors, ranch_tcp, [{port, Port}],
																 erwa_in_tcp, []),
	ok.




%% for router


%% @doc Start a router for a realm.
-spec start_realm(Name :: binary() ) -> ok | {error, Reason :: term()}.
start_realm(Name) ->
  ok = erwa_realms:add(Name),
  ok.


%% @doc Stop the router of a realm.
-spec stop_realm(Name :: binary()) -> {ok,Info :: atom()} | {error, Reason :: atom()}.
stop_realm(Name) ->
  erwa_realms:remove(Name).




