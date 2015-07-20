%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% For calling erwa_realms service from other threads
%%% @end
%%% Created : 20. Jul 2015 15:03
%%%-------------------------------------------------------------------
-module(erwa_realms_man).
-author("tihon").

-include("erwa_service.hrl").

%% API
-export(
[
  get_middleware_list/1,
  get_routing/1,
  add/1,
  add/2,
  kill/1,
  shutdown/1,
  set_autocreate/1
]).

-spec get_middleware_list(Name :: binary()) -> {ok, [atom()]} | {error, not_found}.
get_middleware_list(Name) ->
  get_realm_data(middleware, Name).

-spec get_routing(Name :: binary()) -> {ok, Realm :: pid()} | {error, not_found}.
get_routing(Name) ->
  get_realm_data(routing, Name).

-spec add(Name :: binary()) -> ok | {error, Reason :: term()}.
add(Name) ->
  MW_List = application:get_env(erwa, router_middleware, [erwa_mw_default]),
  add(Name, MW_List).

-spec add(Name :: binary(), Middlewares :: [atom()]) -> ok | {error, Reason :: term()}.
add(Name, Middlewares) ->
  gen_server:call(?REALMS_SERVICE, {start_realm, Name, Middlewares}).

-spec kill(Name :: binary()) -> {ok, killing} | {error, Reason :: term()}.
kill(Name) ->
  gen_server:call(?REALMS_SERVICE, {kill_realm, Name}).

-spec shutdown(Name :: binary()) -> {ok, shutting_down} | {error, Reason :: term()}.
shutdown(Name) ->
  gen_server:call(?REALMS_SERVICE, {shutdown_realm, Name}).

-spec set_autocreate(boolean()) -> ok.
set_autocreate(true) ->
  gen_server:call(?REALMS_SERVICE, enable_autocreate);
set_autocreate(false) ->
  gen_server:call(?REALMS_SERVICE, disable_autocreate).


%% @private
%% @doc
%% Get realm data. If realm not found and autocreate is enabled - create it
%% and get it's data then.
-spec get_realm_data(middleware | routing, term()) -> term().
get_realm_data(Tag, Name) ->
  case ets:lookup(?REALMS_ETS, Name) of
    [{Name, active, Pid, _Ref, MW_List}] ->
      case Tag of
        middleware ->
          {ok, MW_List};
        routing ->
          {ok, Pid}
      end;
    [{Name, _, _Pid, _Ref, _MW_List}] ->
      {error, shutting_down};
    [] ->
      try_autocreate(Tag, Name)
  end.

%% @private
%% Call erwa_realms server if autocreate enabled
try_autocreate(Tag, Name) ->
  case application:get_env(erwa, realm_autocreate, false) of
    true ->
      MW_List = application:get_env(erwa, router_middleware, [erwa_mw_default]),
      ok = add(Name, MW_List),
      get_realm_data(Tag, Name);
    false ->
      {error, not_found}
  end.