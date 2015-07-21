%%
%% Copyright (c) 2015 Bas Wegh
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

-module(erwa_user_db).
-behaviour(gen_server).

%% API
-export([start/0]).
-export([start_link/0]).
%-export([stop/0]).

%% for authentication 
-export([can_join/3]).
-export([allow_anonymous/2]).
-export([wampcra_challenge/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  userdb = none,
  permdb = none
}).

-record(user, {
  user_id = unknown,
  secret = none,
  salt = none,
  keylen = 0,
  iterations = 0,
  roles = []
}).

-record(perm, {
  realm_role_trans = {none, none, any},
  static = undefined,
  dyn = undefined
}).


-define(USERTAB, erwa_user_db_user_tab).
-define(PERMTAB, erwa_user_db_perm_tab).

-spec start() -> {ok, pid()}.
start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec allow_anonymous(Realm :: binary(), Transport :: atom()) -> true | false.
allow_anonymous(Realm, Transport) ->
  %% case can_join(anonymous, Realm, Transport) of 
  %%   {true,_} -> true;
  %%   _ -> false 
  %% end.
  true.

-spec can_join(AuthId :: binary() | anonymous, Realm :: binary(),
    Transport :: atom()) ->
  {true, Role :: binary()} | false.
can_join(AuthId, Realm, Transport) ->
  case ets:lookup(?USERTAB, AuthId) of
    [] -> false;
    [#user{roles = PossibleRoles}] ->
      case get_role(PossibleRoles, Realm, Transport) of
        none -> false;
        Role -> {true, Role}
      end
  end.

-spec wampcra_challenge(SessionData :: map()) -> {atom(), map()}.
wampcra_challenge(#{authid := AuthId, role := Role, session := SessionNbr}) ->
  case ets:lookup(?USERTAB, AuthId) of
    [#user{salt = Salt, keylen = KeyLen, iterations = Iterations}] ->
      {ok, create_wampcra_data(AuthId, Role, SessionNbr, Salt, KeyLen,
        Iterations)};
    [] ->
      {error, create_wampcra_data(AuthId, Role, SessionNbr, none, 0, 0)}
  end.

create_wampcra_data(AuthId, Role, SessionNbr, Salt, KeyLen,
    Iterations) ->
  AuthProvider = <<"erwa_user_db">>,
  {{Year, Month, Day}, {Hour, Minute, Seconds}} = calendar:universal_time(),
  Timestamp = list_to_binary(io_lib:format("~.10B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.000Z", [Year, Month, Day, Hour, Minute, Seconds])),

  Challenge = jsx:encode([{<<"nonce">>, erwa_support:nonce()}, {<<"authprovider">>, AuthProvider},
    {<<"authid">>, AuthId}, {<<"timestamp">>, Timestamp},
    {<<"authrole">>, Role}, {<<"authmethod">>, <<"wampcra">>},
    {<<"session">>, SessionNbr}]),
  case Salt of
    none ->
      #{challenge => Challenge};
    _ ->
      #{challenge => Challenge, salt => Salt, keylen => KeyLen, iterations =>
      Iterations}
  end.


%% gen_server
init([]) ->
  UserTab = ets:new(?USERTAB, [set, {keypos, 2}, named_table]),
  PermTab = ets:new(?PERMTAB, [set, {keypos, 2}, named_table]),
  {ok, #state{userdb = UserTab, permdb = PermTab}}.

handle_call({add_user, AuthId, Secret, Salt, Iterations, KeyLen, Roles}, _From,
    #state{userdb = Udb} = State) ->
  case ets:insert_new(Udb, #user{user_id = AuthId, secret = Secret, salt = Salt,
    keylen = KeyLen, iterations = Iterations,
    roles = Roles}) of
    true -> {reply, ok, State};
    false -> {reply, {error, user_exists}, State}
  end;
handle_call({add_user_role, AuthId, Role}, _From, #state{userdb = Udb} = State) ->
  case ets:lookup(Udb, AuthId) of
    [] -> {reply, {error, user_doesnt_exist}, State};
    [#user{roles = Roles} = User] ->
      true = ets:insert(Udb, User#user{roles = [Role | Roles]}),
      {reply, ok, State}
  end;
handle_call({add_static_perm, Realm, Role, Transport, Uri, Publish, Subscribe,
  Call, Register}, _From, #state{permdb = PermDb} = State) ->
  case ets:insert_new(PermDb,
    #perm{realm_role_trans = {Realm, Role, Transport},
      static = #{uri =>Uri,
        publish => Publish, subscribe => Subscribe,
        call => Call, register => Register}
    }) of
    true -> {reply, ok, State};
    false -> {reply, {error, already_exists}, State}
  end;
handle_call({add_dyn_perm, Realm, Role, Transport,
  Uri}, _From, #state{permdb = PermDb} = State) ->
  case ets:insert_new(PermDb,
    #perm{realm_role_trans = {Realm, Role, Transport},
      dyn = Uri}) of
    true -> {reply, ok, State};
    false -> {reply, {error, already_exists}, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @private
get_role([], _, _) -> none;
get_role([Role | Roles], Realm, Transport) ->
  case ets:member(?PERMTAB, {Role, Realm, Transport}) of
    true -> Role;
    false -> get_role(Roles, Realm, Transport)
  end.