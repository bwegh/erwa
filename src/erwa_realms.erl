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

-module(erwa_realms).

-export([init/0]).
-export([add/1]).
-export([remove/1]).

-export([exists/1]).

-record(erwa_realm, {
          name = unknown,
          state = running
         }).

-spec add(Name :: binary() ) -> ok | {error, Reason :: term() }.
add(Name) ->
    F = fun() -> 
                case mnesia:read(erwa_realms, Name) of 
                    [] ->
                        ok = mnesia:write(erwa_realms, #erwa_realm{name=Name}, write),
                        ok;
                    _ -> 
                        {error, already_exists}
                end 
        end,
    {atomic, Res} =  mnesia:transaction(F),
    case Res of
        ok ->
            ok = erwa_broker:init(Name),
            ok = erwa_dealer:init(Name),
            {ok, _CalleePid} = erwa_callee:start(#{realm=>Name})
    end,
    Res.

-spec remove(Name :: binary()) -> {ok, removing} | {error, Reason :: term()}.
remove(Name) ->
    F = fun() ->
                case mnesia:read(erwa_realms, Name) of
                    [] ->
                        {error, not_running};
                    [#erwa_realm{}] ->
                        ok = mnesia:delete(erwa_realms,Name,write),
                        {ok, removed};
                    Other ->
                        {error,Other}
                end 
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

-spec exists(Realm::binary()) -> true | false.
exists(Realm) ->
    F = fun() ->
                case mnesia:read(erwa_realms, Realm) of 
                    [] -> false;
                    [#erwa_realm{}] -> true 
                end 
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.



init() ->
    ok = create_table(),
    ok.



create_table() ->
    Table = erwa_realms,
	case lists:member(Table, mnesia:system_info(local_tables)) of
		true ->
			mnesia:delete_table(Table);
		_-> do_nothing
	end,
	{atomic, ok} = mnesia:create_table(Table, [
                                               {disc_copies, []},
                                               {ram_copies, [node()]}, 
                                               {type, set},
                                               {record_name, erwa_realm},
                                               {attributes, record_info(fields, erwa_realm)}
                                              ]),
	ok.


%% -ifdef(TEST).
%%
%% get_tablesize() ->
%%   Pid = whereis(?MODULE),
%%   Tables = ets:all(),
%%   [Table] = lists:filter(fun(T) -> ets:info(T,owner) == Pid end,Tables),
%%   ets:info(Table,size).
%%
%%
%% ensure_tablesize(_Number,MaxTime) when MaxTime =< 0 ->
%%   timeout;
%% ensure_tablesize(Number,MaxTime) ->
%%   case get_tablesize() of
%%     Number -> ok;
%%     _ ->
%%       receive
%%       after 10 -> ok
%%       end,
%%       NewTime = MaxTime - 10,
%%       ensure_tablesize(Number,NewTime)
%%   end.
%%
%% start_stop_test() ->
%%   {ok,_}=start(),
%%   {ok,stopped}=stop().
%%
%% environment_test() ->
%%   application:set_env(erwa,router_middleware,[erwa_mw_allow]),
%%   [erwa_mw_allow] = application:get_env(erwa,router_middleware,[erwa_mw_default]),
%%   application:unset_env(erwa,router_middleware),
%%   [erwa_mw_default] = application:get_env(erwa,router_middleware,[erwa_mw_default]).
%%
%% garbage_test() ->
%%   {ok,Pid} = start(),
%%   ignored = gen_server:call(?MODULE,some_garbage),
%%   ok = gen_server:cast(?MODULE,some_garbage),
%%   Pid ! some_garbage,
%%   {ok,stopped} = stop().
%%
%% add_remove_test() ->
%% 	ok = erwa_sess_man:create_table(),
%% 	{ok,_} = start(),
%%   Name1 = <<"com.doesnotexist.wamp">>,
%%   Name2 = <<"com.doesnotexist.pamw">>,
%%   MWL = [erwa_mw_allow],
%%   ok = set_autocreate(false),
%%   0 = get_tablesize(),
%%   {error,not_found} = get_routing(Name1),
%%   0 = get_tablesize(),
%%   {error,not_running} = kill(Name1),
%%   0 = get_tablesize(),
%%   ok = add(Name1),
%%   2 = get_tablesize(),
%%   {error,already_exists} = add(Name1),
%%   2 = get_tablesize(),
%%   ok = add(Name2,MWL),
%%   4 = get_tablesize(),
%%   {ok,_} = get_routing(Name1),
%%   {ok,_} = get_middleware_list(Name1),
%%   {ok,_} = get_routing(Name2),
%%   {ok,killed} = kill(Name1),
%%   ok = ensure_tablesize(2,5000),
%%   {error,not_found} = get_routing(Name1),
%%   {ok,shutting_down} = shutdown(Name2),
%%   ok = ensure_tablesize(0,5000),
%%   {error,not_found} = get_routing(Name2),
%%   ok = set_autocreate(true),
%%   {ok,_} = get_routing(Name1),
%%   ok = set_autocreate(false),
%%   2 = get_tablesize(),
%%   {ok,killed} = kill(Name1),
%%   ok = ensure_tablesize(0,5000),
%%   timeout = ensure_tablesize(5,10),
%% 	ok = erwa_sess_man:drop_table(),
%%   {ok,stopped} = stop().
%%
%%
%%
%%
%%
%% -endif.
