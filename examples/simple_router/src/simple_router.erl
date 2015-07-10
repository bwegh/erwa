%% do with this code what ever you like
%% Bas Wegh

%% @private
-module(simple_router).
-behaviour(supervisor).

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
  erwa_realms:set_autocreate(true),
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/", cowboy_static, {priv_file, simple_router, "index.html"}},
                                           {"/wamp", erwa_in_ws, []},
                                           {"/static/[...]", cowboy_static, {priv_dir, simple_router, "static"}}
                                           ]}
                                    ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[{env, [{dispatch, Dispatch}]}]),
  {ok,_} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_in_tcp, []),
  {ok, {{one_for_one, 10, 10}, []}}.
