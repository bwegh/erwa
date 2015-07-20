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

%% @TODO: add ssl check

-module(erwa_in_tcp).

-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("erwa_model.hrl").

%% for tcp
-export([start_link/4]).
-export([init/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


% (x+9) ** 2 is the lengh
% so
%   0 -> 2 ** 9
%   1 -> 2 ** 10 etc.
% the number gets shifted 4 bits to the left.
% 15 is the max receive length possible ~ 16M (2 ** 24).
-define(MAXLENGTH, 15).

%%% for TCP

start_link(Ref, Socket, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, []) ->
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  {Ok, Closed, Error} = Transport:messages(),
  ok = Transport:setopts(Socket, [{active, once}]),
  Session = #session{},
  ErlBinNumber = application:get_env(erwa, erlbin_number, undefined),
  State = #tcp_state{socket = Socket, transport = Transport,
    ok = Ok, closed = Closed, error = Error,
    session = Session, erlbin_number = ErlBinNumber},
  gen_server:enter_loop(?MODULE, [], State).

%%%% TCP - gen_server

init(_Opts) ->
  erlang:error("don't call").

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

%%% incomming TCP %%%%%%
handle_info({OK, Socket, <<127, L:4, S:4, 0, 0>>}, State = #tcp_state{ok = OK, socket = Socket,
  transport = Transport, enc = undefined, session = Session, erlbin_number = EBin}) ->
  % there is no special case for extra data comming along the line as this is against the spec.
  % the client waits for the reply ... if not then kill the line.
  MaxLength = round(math:pow(2, 9 + L)),
  {Enc, Reply} = case S of
                   0 ->
                     {illegal, <<127, 0, 0, 0>>};
                   1 ->
                     {raw_json, <<127, ?MAXLENGTH:4, S:4, 0, 0>>};
                   2 ->
                     {raw_msgpack, <<127, ?MAXLENGTH:4, S:4, 0, 0>>};
                   EBin ->
                     {raw_erlbin, <<127, ?MAXLENGTH:4, S:4, 0, 0>>};
                   _ ->
                     {undefined, <<127, 0, 0, 0>>}
                 end,

  {ok, Peer} = erwa_support:get_peer(Socket),
  Transport:send(Socket, Reply),
  case Enc of
    illegal -> {stop, normal, State};
    undefined -> {stop, normal, State};
    _ ->
      Transport:setopts(Socket, [{active, once}]),
      USession = Session#session{source = tcp, peer = Peer},
      {noreply, State#tcp_state{enc = Enc, length = MaxLength, session = USession}}
  end;
handle_info({OK, Socket, <<127, _L:4, _S:4, _, _>>},
    State = #tcp_state{ok = OK, socket = Socket, transport = Transport, enc = undefined}) ->
  % stop any connection that is not sending the last two zeros.
  Transport:send(Socket, <<127, 0, 0, 0>>),
  {stop, normal, State};
handle_info({OK, Socket, _}, State = #tcp_state{ok = OK, socket = Socket, enc = undefined}) ->
  % just close any misbehaving client
  {stop, normal, State};
handle_info({OK, Socket, Data},
    State = #tcp_state{ok = OK, socket = Socket, transport = Transport, enc = Enc, buffer = Buffer, session = Session}) ->
  Transport:setopts(Socket, [{active, once}]),
  {Messages, NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>, Enc),
  case handle_messages(Messages, Session, Transport, Socket, Enc) of
    {ok, NewSession} ->
      NewState = State#tcp_state{session = NewSession, buffer = NewBuffer},
      {noreply, NewState};
    {stop, NewSession} ->
      NewState = State#tcp_state{session = NewSession, buffer = NewBuffer},
      {stop, normal, NewState}
  end;
handle_info({Closed, Socket}, #tcp_state{closed = Closed, socket = Socket} = State) ->
  %wamper_protocol:close(connection_closed,ProtState),
  {stop, normal, State};
handle_info({Error, Socket, Reason}, #tcp_state{error = Error, socket = Socket} = State) ->
  {stop, {error, Reason}, State};
%%% messages comming from routing / system (internal) %%%
handle_info({erwa, Msg},
    State = #tcp_state{socket = Socket, transport = Transport, enc = Enc, session = Session}) when is_tuple(Msg) ->
  case erwa_session:handle_info(Msg, Session) of
    {ok, NewSession} ->
      {noreply, State#tcp_state{session = NewSession}};
    {send, OutMsg, NewSession} ->
      Transport:send(Socket, wamper_protocol:serialize(OutMsg, Enc)),
      {noreply, State#tcp_state{session = NewSession}};
    {send_stop, OutMsg, NewSession} ->
      Transport:send(Socket, wamper_protocol:serialize(OutMsg, Enc)),
      {stop, normal, State#tcp_state{session = NewSession}};
    {stop, NewSession} ->
      {stop, normal, State#tcp_state{session = NewSession}}
  end;
handle_info(Info, State) ->
  erlang:error("tcp_in: unknown info message: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @private
handle_messages([], Session, _Transport, _Socket, _Enc) ->
  {ok, Session};
handle_messages([Msg | Tail], Session, Transport, Socket, Enc) ->
  case erwa_session:handle_message(Msg, Session) of
    {ok, NewSession} ->
      handle_messages(Tail, NewSession, Transport, Socket, Enc);
    {reply, OutMsg, NewSession} ->
      Transport:send(Socket, wamper_protocol:serialize(OutMsg, Enc)),
      handle_messages(Tail, NewSession, Transport, Socket, Enc);
    {reply_stop, OutMsg, NewSession} ->
      Transport:send(Socket, wamper_protocol:serialize(OutMsg, Enc)),
      {stop, NewSession};
    {stop, NewSession} ->
      {stop, NewSession}
  end.