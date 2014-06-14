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
               prot_state = undefined
              }).

-define(TIMEOUT,60000).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    %% Perform any required state initialization here.
    ProtState = erwa_protocol:create(),

    ok = ranch:accept_ack(Ref),
    {Ok,Closed, Error} = Transport:messages(),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket,transport=Transport,ok=Ok,closed=Closed,error=Error,prot_state=ProtState}).


init(_Opts) ->
  erlang:error("never call this").


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({OK,Socket,Data},  #state{ok=OK,socket=Socket,transport=Transport,buffer=Buf,prot_state=ProtState}=State) ->
  Buffer = <<Buf/binary, Data/binary>>,
  {NewBuffer,Messages} = get_messages_from_buffer(Buffer),
  {ok,NewProtState} = handle_messages(Messages,Socket,Transport,ProtState),
  {noreply, State#state{prot_state=NewProtState,buffer=NewBuffer}};
handle_info({Closed,Socket}, #state{closed=Closed,socket=Socket}=State) ->
  %erwa_protocol:close(connection_closed,ProtState),
	{stop, normal, State};
handle_info({Error,Socket,Reason}, #state{error=Error,socket=Socket}=State) ->
	{stop, {error, Reason} , State};
handle_info({erwar,shutdown}, State) ->
  {stop, normal, State};
handle_info({erwa,Msg}, #state{socket=Socket,transport=Transport}=State) ->
  Transport:send(Socket,encode(Msg)),
  {noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.


terminate(Reason, #state{prot_state=ProtState}) ->
  erwa_protocol:close(Reason,ProtState),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



get_messages_from_buffer(Data) ->
  get_messages_from_buffer(Data,[]).

get_messages_from_buffer(<<"">>,Messages) ->
  {<<"">>,lists:reverse(Messages)};
get_messages_from_buffer(<<Len:32,Data/binary>>=Buffer,Messages)  ->
  case byte_size(Data) >= Len of
    true ->
      <<Enc:Len/binary,NewBuffer/binary>> = Data,
      get_messages_from_buffer(NewBuffer,[decode(Enc)|Messages]);
    false ->
      {Buffer,Messages}
  end.

-spec handle_messages(Messages :: list(), Socket :: any(), Transport :: atom(), ProtState :: any()) -> {ok,any()} | {error,any()}.
handle_messages([],_Socket,_Transport,ProtState) ->
  {ok,ProtState};
handle_messages([M | T ],Socket,Transport,ProtState) ->
  {ok,NewProt} = handle_message(M,Socket,Transport,ProtState),
  handle_messages(T,Socket,Transport,NewProt).

handle_message(Message,Socket,Transport,ProtState)  ->
  {ok,Reply,NewProt} = erwa_protocol:handle(Message,ProtState),
  case Reply of
    noreply ->
      ok;
    shutdown ->
      Transport:close(Socket);
    Reply ->
      Transport:send(Socket,encode(Reply))
  end,
  {ok,NewProt}.



decode(Message) ->
  decode(Message,json).
decode(Message,json) ->
  jsx:decode(Message);
decode(Message,msgpack) ->
  {ok,Msg} = msgpack:unpack(Message,[jsx]),
  Msg.


encode(Message) ->
  encode(Message,json).
encode(Message,json) ->
  Enc = jsx:encode(Message),
  Len = byte_size(Enc),
  <<Len:32,Enc/binary>>;
encode(Message,msgpack) ->
  Enc = msgpack:pack(Message,[jsx]),
  Len = byte_size(Enc),
  <<Len:32,Enc/binary>>.



-ifdef(TEST).

buffer_test() ->
  Msg1 = [1,<<"my.test-realm.com">>,[{<<"message">>,<<"hope this works">>}]],
  Enc1 = encode(Msg1),
  Msg2 = [6,[{}],<<"wamp.error.system_shutdown">>],
  Enc2 = encode(Msg2),
  Data = <<Enc1/binary,Enc2/binary>>,
  Result = get_messages_from_buffer(Data),
  Expected = {<<"">>,[Msg1,Msg2]},
  Result = Expected.

-endif.

