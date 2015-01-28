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

-module(erwa_mw_allow).
-behaviour(erwa_middleware).

-export([perm_connect/3]).
-export([authenticate/3]).
-export([perm_publish/5]).
-export([perm_subscribe/3]).
-export([perm_call/5]).
-export([perm_register/3]).

perm_connect(_SessionId, _Realm, _Details) ->
  true.


authenticate(_SessionId, _Signature, _Extra) ->
  true.

perm_publish(_SessionId, _Options, _Topic, _Arguments, _ArgumentsKw) ->
  true.

perm_subscribe(_SessionId, _Options, _Topic) ->
  true.

perm_call(_SessionId, _Options, _Procedure, _Arguments, _ArgumentsKw) ->
  true.

perm_register(_SessionId, _Options, _Procedure) ->
  true.
