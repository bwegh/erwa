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

-module(erwa_auth).

-export([wamp_cra/2]).
-export([pbkdf2/4]).

%% @doc calculates the cryptographic hash of the challenge by using the secret key.
-spec wamp_cra(Key :: binary(), Challenge :: binary() ) -> binary().
wamp_cra(Key,Challenge) ->
  _Bin = crypto:hmac(sha256,Key,Challenge),
  %% @todo need to convert to ascii ... how?!
  <<"bigFail">>.


%% @doc calculates the derived key from secret key, using salt and iterations.
-spec pbkdf2(SecretKey :: binary(), Salt :: binary(),
                      Iterations :: non_neg_integer(),
                      Length :: non_neg_integer()) -> {ok, NewKey :: binary()}.
pbkdf2(SecretKey, Salt, Iterations, Length) ->
  pbkdf2:pbkdf2(SecretKey, Salt, Iterations, Length).
