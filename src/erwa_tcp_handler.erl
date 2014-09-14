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

%% @private
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
               enc = undefined,
               length = infitity,
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


handle_info({OK,Socket,<<127,L:4,S:4,0,0>>},  #state{ok=OK,socket=Socket,transport=Transport,enc=undefined}=State) ->
  Transport:setopts(Socket, [{active, once}]),
  MaxLength = math:pow(2,9+L),
  % 15 is the max receive length possible ~ 16M
  Byte = (15 bsl 4)  bor S,
  {Enc,Reply} = case S of
                  0 ->
                    {illegal,<<127,0,0,0>>};
                  1 ->
                    {raw_json,<<127,Byte,0,0>>};
                  2 ->
                    {raw_msgpack,<<127,Byte,0,0>>};
                  _ ->
                    {undefined,<<127,0,0,0>>}
                end,
  Transport:send(Socket,Reply),
  case Enc of
    illegal -> {stop,normal,State};
    undefined -> {stop,normal,State};
    _ -> {noreply,State#state{enc=Enc,length=MaxLength}}
  end;
handle_info({OK,Socket,<<127,_L:4,_S:4,_Bits:16>>},  #state{ok=OK,socket=Socket,transport=Transport,enc=undefined}=State) ->
  Byte = 2 bsl 4,
  Transport:send(Socket,<<127,Byte,0,0>>),
  {stop,normal,State};
handle_info({OK,Socket,Data},  #state{ok=OK,enc=Enc,socket=Socket,transport=Transport,buffer=Buf,router=Router}=State) ->
  Transport:setopts(Socket, [{active, once}]),
  Buffer = <<Buf/binary, Data/binary>>,
  {Messages,NewBuffer} = erwa_protocol:deserialize(Buffer,Enc),
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

raw_init_test() ->
  <<127,Length:4,Serializer:4,0,0>> = <<127,242,0,0>>,
  Length = 15,
  Serializer = 2.

-endif.

