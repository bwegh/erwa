%% do with this code what ever you like
%% Bas Wegh

%% @private
-module(simple_conn_between_erlang_and_web_client).
-behaviour(supervisor).

%% ====================================================================
%% Defines
%% ====================================================================
-define(WEB_HTTP_PORT, 8080).
-define(WEB_HTTPS_PORT, 8443).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link( ?MODULE, []).

%% supervisor.

init([]) ->
	AppName = simple_conn_between_erlang_and_web_client,
	
	erwa_realms:set_autocreate(true),
	
	Dispatch = cowboy_router:compile([
                                    {'_', [
										   {"/", cowboy_static, {priv_file, AppName, "index.html"}},
										   {"/wamp", erwa_in_ws, []},
										   {"/static/[...]", cowboy_static, {priv_dir, AppName, "static"}}
										  ]}
                                    ]),
	
	%% This section is for HTTP
	{ok, _HttpPid} = cowboy:start_http(http, 100, [{port, ?WEB_HTTP_PORT}],
									  [{env, [{dispatch, Dispatch}]}]),
	
	%% This section is for HTTPS
	PrivDir = code:priv_dir(AppName),
	{ok, _HttpsPid} = cowboy:start_https(https, 100, [
													 {port, ?WEB_HTTPS_PORT},
													 {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
													 {certfile, PrivDir ++ "/ssl/server.crt"},
													 {keyfile, PrivDir ++ "/ssl/server.key"}
													], 
										[{env, [{dispatch, Dispatch}]}]),
	
	%%{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[{env, [{dispatch, Dispatch}]}]),
	
	{ok,_} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_in_tcp, []),
	{ok, {{one_for_one, 10, 10}, []}}.
