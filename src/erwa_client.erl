%%
%% Copyright (c) 2014 Bas Wegh
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

-module(erwa_client).

-export([is_valid_client_module/1]).
-export([is_valid_event/2]).
-export([is_valid_rpc/2]).


-type registration() :: {ProcedureUrl :: binary(), Options :: list(), Function :: atom()}.
-type subscription() :: {TopicUrl :: binary(), Options :: list(), Function :: atom() }.
-type state() :: any().
-type connection() :: any().

-export_type([registration/0,subscription/0]).


-callback init(any()) -> {ok,state()}.
-callback on_connect(state(),connection()) -> {ok,state()}.
%-callback on_error(non_neg_integer(),non_neg_integer(),state(),connection()) -> {ok,state()}.
-callback on_result(non_neg_integer(),list(),list(),list(),state(),connection()) -> {ok,state()}.

%% @doc Test whether a module exists and exports all needed functions
is_valid_client_module(Module) when is_atom(Module) ->
  true =
    try Module:module_info() of
      _InfoList ->
        true
    catch
      _:_ ->
        false
    end,
  Exports = Module:module_info(exports),
  true = lists:member({init,1},Exports),
  true = lists:member({on_connect,2},Exports),
  true = lists:member({on_result,6},Exports),
  true.

is_valid_rpc(Module,Method) when is_atom(Module), is_atom(Method) ->
  Exports = Module:module_info(exports),
  true = lists:member({Method,5},Exports).

is_valid_event(Module,Method) when is_atom(Module), is_atom(Method) ->
  Exports = Module:module_info(exports),
  true = lists:member({Method,6},Exports).

