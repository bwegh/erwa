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
  elogger_config:set_loglevel(debug),
  erwa:start_realm(<<"realm1">>),
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/", cowboy_static, {priv_file, simple_router, "index.html"}},
                                           {"/wamp", erwa_in_ws, []},
                                           {"/static/[...]", cowboy_static, {priv_dir, simple_router, "static"}}
                                           ]}
                                    ]),
  
  {ok, CowboyVsn} = application:get_key(cowboy,vsn),
  case string:str(CowboyVsn, "1.") of
	  1 ->
		  %% This is for cowboy-1.x
		  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[{env, [{dispatch, Dispatch}]}]);
	  _->
		  %% This is for cowboy-2.x or newer
		  {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}],#{env => #{dispatch => Dispatch}})
  end,
  
  {ok,_} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_in_tcp, []),
  {ok, {{one_for_one, 10, 10}, []}}.
