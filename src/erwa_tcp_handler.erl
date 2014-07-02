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

-module(erwa_tcp_handler).


-behaviour(ranch_protocol).
-behaviour(gen_server).


-export([start_link/4]).
-export([init/4]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



-record(state,{
               socket,
               transport,
               type,
               ok,
               closed,
               error,
               buffer = <<"">>,
               router = undefined
              }).

-define(TIMEOUT,60000).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    {Ok,Closed, Error} = Transport:messages(),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket,transport=Transport,ok=Ok,closed=Closed,error=Error}).


init(_Opts) ->
  erlang:error("don't call").

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({OK,Socket,Data},  #state{ok=OK,socket=Socket,transport=Transport,buffer=Buf,router=Router}=State) ->
  Transport:setopts(Socket, [{active, once}]),
  Buffer = <<Buf/binary, Data/binary>>,
  {Messages,NewBuffer} = erwa_protocol:deserialize(Buffer,raw_msgpack),
  {ok,NewRouter} = erwa_protocol:forward_messages(Messages,Router),
  {noreply, State#state{buffer=NewBuffer,router=NewRouter}};
handle_info({Closed,Socket}, #state{closed=Closed,socket=Socket}=State) ->
  %erwa_protocol:close(connection_closed,ProtState),
  {stop, normal, State};
handle_info({Error,Socket,Reason}, #state{error=Error,socket=Socket}=State) ->
  {stop, {error, Reason} , State};
handle_info({erwa,shutdown}, State) ->
  {stop, normal, State};
handle_info({erwa,Msg}, #state{socket=Socket,transport=Transport}=State) when is_tuple(Msg) ->
  Transport:send(Socket,erwa_protocol:serialize(Msg,raw_msgpack)),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.





-ifdef(TEST).

-endif.

