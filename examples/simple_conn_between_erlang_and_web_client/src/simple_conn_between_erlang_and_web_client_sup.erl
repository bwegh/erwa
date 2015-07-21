-module(simple_conn_between_erlang_and_web_client_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([start_client/2]).
-export([start_link/0]).
-export([init/1]).

start_client(Client, Realm) when is_atom(Client), is_list(Realm)->
    supervisor:start_child(?SERVER,
						   {Client, 
							{simple_erlang_client, start_link, [Client, erlang:list_to_binary(Realm)]},
							transient, 10, worker, [simple_erlang_client]}).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?SERVER, []).

init([]) ->
	Procs = [],
	%%Procs = [{simple_erlang_client,{simple_erlang_clientl,start_link,[]},permanent,5000,worker,[]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
