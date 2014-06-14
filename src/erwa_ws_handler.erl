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

-module(erwa_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state,{
  enc = undefined,
  prot = undefined
               }).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  % need to check for the wamp.2.json or wamp.2.msgpack
  {ok, Protocols, Req1} = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req),
  Prot = erwa_protocol:create(),
  case lists:nth(1,Protocols) of
      <<"wamp.2.msgpack">> ->
        Req2  = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,<<"wamp.2.msgpack">>,Req1),
        {ok,Req2,#state{enc=msgpack,prot=Prot}};
      <<"wamp.2.json">> ->
        Req2  = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,<<"wamp.2.json">>,Req1),
        {ok,Req2,#state{enc=json,prot=Prot}};
      _ ->
        {shutdown,Req1}
  end.


websocket_handle({text, Data}, Req, #state{enc=json}=State) ->
  handle_wamp(Data,Req,State);

websocket_handle({binary, Data}, Req, #state{enc=msgpack}=State) ->
  handle_wamp(Data,Req,State);

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({erwar,shutdown}, Req, State) ->
  {shutdown,Req,State};
websocket_info({erwa,Msg}, Req, #state{enc=Enc}=State) ->
  {reply, encode(erwa_protocol:to_wamp(Msg),Enc), Req, State};
websocket_info(_Data, Req, State) ->
  {ok,Req,State}.

websocket_terminate(Reason, _Req, #state{prot=Prot}) ->
  erwa_protocol:close(Reason,Prot),
  ok.


handle_wamp(Data,Req,#state{prot=Prot, enc=Enc}=State) ->
  Msg = decode(Data,Enc),
  {ok,Reply,NewProt} = erwa_protocol:handle(Msg,Prot),
  NewState = State#state{prot=NewProt},
  case Reply of
    noreply -> {ok, Req, NewState};
    shutdown -> {shutdown,Req,NewState};
    Reply -> {reply, encode(Reply,Enc),Req,NewState}
  end.

decode(Data,msgpack) ->
    {ok,Msg} = msgpack:unpack(Data,[{format,jsx}]),
    Msg;
decode(Data,json) ->
    jsx:decode(Data).


encode(Msg,_Enc) when is_atom(Msg) ->
  Msg;
encode(Msg,msgpack)  ->
  {binary,msgpack:pack(Msg,[{format,jsx}])};
encode(Msg,json)  ->
  {text,jsx:encode(Msg)}.


-ifdef(TEST).




-endif.
