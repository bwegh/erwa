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
-module(erwa_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(SUBPROTHEADER,<<"sec-websocket-protocol">>).
-define(WSMSGPACK,<<"wamp.2.msgpack">>).
-define(WSJSON,<<"wamp.2.json">>).

-record(state,{
  enc = undefined,
  router = undefined,
  buffer = <<"">>
               }).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  % need to check for the wamp.2.json or wamp.2.msgpack
  {ok, Protocols, Req1} = cowboy_req:parse_header(?SUBPROTHEADER, Req),
  case lists:nth(1,Protocols) of
      ?WSMSGPACK ->
        Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,?WSMSGPACK,Req1),
        {ok,Req2,#state{enc=msgpack}};
      ?WSJSON ->
        Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,?WSJSON,Req1),
        {ok,Req2,#state{enc=json}};
      _ ->
        {shutdown,Req1}
  end.


websocket_handle({text, Data}, Req, #state{enc=json}=State) ->
  {ok,NewState} = handle_wamp(Data,State),
  {ok,Req,NewState};
websocket_handle({binary, Data}, Req, #state{enc=msgpack}=State) ->
  {ok,NewState} = handle_wamp(Data,State),
  {ok,Req,NewState};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({erwa,shutdown}, Req, State) ->
  {shutdown,Req,State};
websocket_info({erwa,Msg}, Req, #state{enc=Enc}=State) when is_tuple(Msg)->
	Rpl = erwa_protocol:serialize(Msg,Enc),
  Reply =
    case Enc of
      json -> {text,Rpl};
      msgpack -> {binary,Rpl}
    end,
	{reply,Reply,Req,State};
websocket_info(_Data, Req, State) ->
  {ok,Req,State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.


handle_wamp(Data,#state{buffer=Buffer, enc=Enc, router=Router}=State) ->
  {Messages,NewBuffer} = erwa_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
  {ok,NewRouter} = erwa_protocol:forward_messages(Messages,Router),
  {ok,State#state{router=NewRouter,buffer=NewBuffer}}.

-ifdef(TEST).




-endif.
