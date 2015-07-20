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

-module(erwa_middleware).

-include("erwa_model.hrl").

-callback perm_connect(Session :: term(), Realm :: binary(), Details :: map()) -> %TODO move all callbacks to interface
  {true, Details :: map()} |
  {false, Details :: map()}.


-callback authenticate(Session :: term(), Signature :: binary(), Extra :: map()) ->
  {true, Details :: map()} |
  {false, Details :: map()}.


-callback perm_publish(Session :: term(), Options :: map(), Topic :: binary(),
    Arguments :: list() | undefined, ArgumentsKw :: map() | undefined) ->
  {true, Details :: map()} |
  {false, Details :: map()}.


-callback perm_subscribe(Session :: term(), Options :: map(), Topic :: binary()) ->
  {true, Details :: map()} |
  {false, Details :: map()}.


-callback perm_call(Session :: term(), Options :: map(), Procedure :: binary(),
    Arguments :: list() | undefined, ArgumentsKw :: map() | undefined) ->
  {true, Details :: map()} |
  {false, Details :: map()}.


-callback perm_register(Session :: term(), Options :: map(), Procedure :: binary()) ->
  {true, Details :: map()} |
  {false, Details :: map()}.


-callback check_out_message(Session :: term(), MessageToBeSent :: term()) ->
  {false, Details :: map()} |
  term().


-export([validate_out_message/2]).
-export([check_perm/2]).

%% @private
check_perm(Msg, Session) ->
  {Method, Args} = case Msg of
                     {hello, RealmName, Details} ->
                       {perm_connect, [Session, RealmName, Details]};
                     {authenticate, Signature, Extra} ->
                       {authenticate, [Session, Signature, Extra]};
                     {subscribe, _RequestId, Options, Topic} ->
                       {perm_subscribe, [Session, Options, Topic]};
                     {publish, _RequestId, Options, Topic, Arguments, ArgumentsKw} ->
                       {perm_publish, [Session, Options, Topic, Arguments, ArgumentsKw]};
                     {register, _RequestId, Options, ProcedureUri} ->
                       {perm_register, [Session, Options, ProcedureUri]};
                     {call, _RequestId, Options, ProcedureUri, Arguments, ArgumentsKw} ->
                       {perm_call, [Session, Options, ProcedureUri, Arguments, ArgumentsKw]};
                     {cancel, _RequestId, Options} ->
                       {perm_cancel, [Session, Options]}
                   end,
  F = fun(MiddleWare, InResult) ->
    case {InResult, apply(MiddleWare, Method, Args)} of
      {{false, OutDetails}, _} ->
        {false, OutDetails};
      {_, Res} ->
        Res
    end
  end,
  lists:foldl(F, {true, #{}}, Session#session.mwl).

%% @private
validate_out_message(Message, Session) ->
  validate_out_message(Message, Session#session.mwl, Session).

%% @private
validate_out_message(false, _, _) -> false;
validate_out_message(Message, [], _) -> Message;
validate_out_message(Message, [MiddleWare | Tail], Session) ->
  ChangedMessage = MiddleWare:check_out_message(Session, Message),
  validate_out_message(ChangedMessage, Tail, Session).