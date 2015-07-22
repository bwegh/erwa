-module(simple_conn_between_erlang_and_web_client_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).
-define(REALM, "realm1").

-export([start_client/1, stop_child/1]).
-export([start_link/0]).
-export([init/1]).

start_client(Client) when is_atom(Client)->
    supervisor:start_child(?SERVER,
						   {Client, 
							{simple_erlang_client, start_link, [Client, erlang:list_to_binary(?REALM)]},
							transient, 10, worker, [simple_erlang_client]}).

stop_child(Client) ->
	supervisor:terminate_child(?SERVER, Client).
	
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?SERVER, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
