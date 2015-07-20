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
-module(erwa_in_tcp_test).
-author("tihon").

-include("erwa_model.hrl").
-include_lib("eunit/include/eunit.hrl").

handshake_hello_test() ->
  meck:new(erwa_support),
  meck:expect(erwa_support, get_peer, fun(_) -> {ok, dummy} end),
  {ok, Socket} = erwa_tcp_mock:start(),
  State = #tcp_state{ok = ok, transport = erwa_tcp_mock, socket = Socket, closed = closed, error = error, session = #session{}},

  {stop, normal, _} = erwa_in_tcp:handle_info({ok, Socket, <<127, 1:4, 0:4, 0, 0>>}, State), %illegal
  {ok, <<127, 0, 0, 0>>} = erwa_tcp_mock:get_buffer(Socket),
  {stop, normal, _} = erwa_in_tcp:handle_info({ok, Socket, <<127, 1:4, 13:4, 0, 0>>}, State), %undefined
  {ok, <<127, 0, 0, 0>>} = erwa_tcp_mock:get_buffer(Socket),
  {stop, normal, _} = erwa_in_tcp:handle_info({ok, Socket, <<127, 1:4, 1:4, 1, 0>>}, State), %very bad
  {ok, <<127, 0, 0, 0>>} = erwa_tcp_mock:get_buffer(Socket),
  {stop, normal, _} = erwa_in_tcp:handle_info({ok, Socket, <<128>>}, State), %very bad
  {ok, <<>>} = erwa_tcp_mock:get_buffer(Socket),

  {noreply, #tcp_state{length = Len1, enc = Enc1}} = erwa_in_tcp:handle_info({ok, Socket, <<127, 1:4, 1:4, 0, 0>>}, State),
  Len1 = 1024,
  Enc1 = raw_json,
  {ok, <<127, 15:4, 1:4, 0, 0>>} = erwa_tcp_mock:get_buffer(Socket),

  {noreply, #tcp_state{length = Len2, enc = Enc2}} = erwa_in_tcp:handle_info({ok, Socket, <<127, 5:4, 2:4, 0, 0>>}, State),
  Len2 = 16384,
  Enc2 = raw_msgpack,
  {ok, <<127, 15:4, 2:4, 0, 0>>} = erwa_tcp_mock:get_buffer(Socket),

  meck:unload(erwa_support),
  erwa_tcp_mock:stop(Socket).
