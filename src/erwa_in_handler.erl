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

%% @private
-module(erwa_in_handler).


-behaviour(cowboy_websocket_handler).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% for tcp
-export([start_link/4]).
-export([init/4]).


%% for websocket
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define(TIMEOUT,60000).

-define(SUBPROTHEADER,<<"sec-websocket-protocol">>).
-define(WSMSGPACK,<<"wamp.2.msgpack">>).
-define(WSJSON,<<"wamp.2.json">>).
-define(WSMSGPACK_BATCHED,<<"wamp.2.msgpack.batched">>).
-define(WSJSON_BATCHED,<<"wamp.2.json.batched">>).


-record(state,{
               socket,
               transport,
               type,
               ok,
               closed,
               error,
               enc = undefined,
               ws_enc = undefined,
               length = infitity,
               buffer = <<"">>,
               routing = undefined,
               source = undefined,
               peer = undefined
              }).


%%% for TCP

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    {Ok,Closed, Error} = Transport:messages(),
    ok = Transport:setopts(Socket, [{active, once}]),
    Routing = erwa_routing:create(),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket,transport=Transport,ok=Ok,closed=Closed,error=Error,routing=Routing}).


%%% for websocket

init({Transport, http}, _Req, _Opts) when Transport == tcp; Transport == ssl ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  % need to check for the wamp.2.json or wamp.2.msgpack
  {ok, Protocols, Req1} = cowboy_req:parse_header(?SUBPROTHEADER, Req),
  case find_supported_protocol(Protocols) of
      {Enc,WsEncoding,Header} ->
        Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,Header,Req1),
        Peer = cowboy_req:peer(Req2),
        Routing = erwa_routing:create(),
        {ok,Req2,#state{enc=Enc,ws_enc=WsEncoding,source=websocket,peer=Peer,routing=Routing}};
      _ ->
        % unsupported
        {shutdown,Req1}
  end.


websocket_handle({WsEnc, Data}, Req, #state{ws_enc=WsEnc}=State) ->
  {ok,NewState} = handle_wamp(Data,State),
  {ok,Req,NewState};
websocket_handle(Data, Req, State) ->
  erlang:error(unsupported,[Data,Req,State]),
  {ok, Req, State}.

websocket_info({erwa,shutdown}, Req, State) ->
  {shutdown,Req,State};
websocket_info({erwa,Msg}, Req, #state{enc=Enc,ws_enc=WsEnc}=State) when is_tuple(Msg)->
	Reply = erwa_protocol:serialize(Msg,Enc),
	{reply,{WsEnc,Reply},Req,State};
websocket_info(_Data, Req, State) ->
  {ok,Req,State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.


handle_wamp(Data,#state{buffer=Buffer, enc=Enc, source=Source, peer=Peer,routing=Routing}=State) ->
  {MList,NewBuffer} = erwa_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
  Messages =
    case length(MList) > 0 of
      true ->
        [First | Others] = MList,
        case First of
          {hello,Realm,Details} ->
            [{hello,Realm,[{erwa,[{source,Source},{peer,Peer}]}|Details]} | Others];
          _ ->
            MList
        end;
      _ ->
        MList
    end,
  {ok,NewRouting} = erwa_routing:handle_messages(Messages,Routing),
  {ok,State#state{routing=NewRouting,buffer=NewBuffer}}.


-spec find_supported_protocol([binary()]) -> atom() | {json|json_batched|msgpack|msgpack_batched,text|binary,binary()}.
find_supported_protocol([]) ->
  none;
find_supported_protocol([?WSJSON|_T]) ->
  {json,text,?WSJSON};
find_supported_protocol([?WSJSON_BATCHED|_T]) ->
  {json_batched,text,?WSJSON_BATCHED};
find_supported_protocol([?WSMSGPACK|_T]) ->
  {msgpack,binary,?WSMSGPACK};
find_supported_protocol([?WSMSGPACK_BATCHED|_T]) ->
  {msgpack_batched,binary,?WSMSGPACK_BATCHED};
find_supported_protocol([_|T]) ->
  find_supported_protocol(T).




%%%% TCP - gen_server

init(_Opts) ->
  erlang:error("don't call").

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({OK,Socket,<<127,L:4,S:4,0,0,Rest/binary>>},
	    #state{ok=OK,socket=Socket,transport=Transport,enc=undefined}=State) ->
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
  {ok,Peer} = inet:peername(Socket),
  case Enc of
    illegal -> {stop,normal,State};
    undefined -> {stop,normal,State};
    _ -> handle_info({OK, Socket, Rest}, State#state{enc=Enc,length=MaxLength,source=tcp,peer=Peer})
  end;
handle_info({OK,Socket,<<127,_L:4,_S:4,_Rest/binary>>},
	    #state{ok=OK,socket=Socket,transport=Transport,enc=undefined}=State) ->
  Byte = 2 bsl 4,
  Transport:send(Socket,<<127,Byte,0,0>>),
  {stop,normal,State};
handle_info({OK,Socket,Data},  #state{ok=OK,socket=Socket,transport=Transport}=State) ->
  Transport:setopts(Socket, [{active, once}]),
  {ok,NewState} = handle_wamp(Data,State),
  {noreply, NewState};
handle_info({Closed,Socket}, #state{closed=Closed,socket=Socket}=State) ->
  %erwa_protocol:close(connection_closed,ProtState),
  {stop, normal, State};
handle_info({Error,Socket,Reason}, #state{error=Error,socket=Socket}=State) ->
  {stop, {error, Reason} , State};
handle_info({erwa,shutdown}, State) ->
  {stop, normal, State};
handle_info({erwa,Msg}, #state{socket=Socket,transport=Transport,enc=Enc}=State) when is_tuple(Msg) ->
  Transport:send(Socket,erwa_protocol:serialize(Msg,Enc)),
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
