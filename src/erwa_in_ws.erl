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
-module(erwa_in_ws).
-compile({parse_transform, lager_transform}).
-behaviour(cowboy_websocket).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% for websocket
% cowboy 1.x
-export([init/3]).
-export([websocket_init/3]).

%cowboy 2.x
-export([init/2]).

% both cowboy versions
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-define(TIMEOUT,60000).

-define(SUBPROTHEADER,<<"sec-websocket-protocol">>).
-define(WSMSGPACK,<<"wamp.2.msgpack">>).
-define(WSJSON,<<"wamp.2.json">>).
-define(WSMSGPACK_BATCHED,<<"wamp.2.msgpack.batched">>).
-define(WSJSON_BATCHED,<<"wamp.2.json.batched">>).


-record(state,{
               enc = undefined,
               ws_enc = undefined,
               length = infitity,
               buffer = <<"">>,
               routing = undefined
              }).

% for cowboy 1.x
init(_, Req, _ ) ->
  {upgrade, protocol, cowboy_websocket, Req, []}.

websocket_init( _Type, Req, _Opts) ->
  {ok,Protocols, Req1} = cowboy_req:parse_header(?SUBPROTHEADER, Req),
  lager:debug("protocols are ~p~n",[Protocols]),
  case find_supported_protocol(Protocols) of
    {Enc,WsEncoding,Header} ->
      Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,Header,Req1),
      %% Peer = cowboy_req:peer(Req1),
      Routing = erwa_routing:init(),
      %% Routing1 = erwa_routing:set_peer(Peer,Routing),
      %% Routing2 = erwa_routing:set_source(websocket,Routing1),
      {ok, Req2, #state{enc=Enc,ws_enc=WsEncoding,routing=Routing}};
    _ ->
      % unsupported
      {shutdown,Req1}
  end.

init( Req, _Opts) ->
  % need to check for the wamp.2.json or wamp.2.msgpack
  Protocols = cowboy_req:parse_header(?SUBPROTHEADER, Req),
  lager:debug("protocols are ~p~n",[Protocols]),
  case find_supported_protocol(Protocols) of
    {Enc,WsEncoding,Header} ->
      Req1  = cowboy_req:set_resp_header(?SUBPROTHEADER,Header,Req),
      %% Peer = cowboy_req:peer(Req1),
      Routing = erwa_routing:init(),
      %% Routing1 = erwa_routing:set_peer(Peer,Routing),
      %% Routing2 = erwa_routing:set_source(websocket,Routing1),
      {cowboy_websocket,Req1,#state{enc=Enc,ws_enc=WsEncoding,routing=Routing}};
    _ ->
      % unsupported
      {shutdown,Req}
  end.



websocket_handle({WsEnc, Data}, Req, #state{ws_enc=WsEnc,enc=Enc,buffer=Buffer,routing=Routing}=State) ->
  {MList,NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
  {ok,OutFrames,NewRouting} = handle_messages(MList,[],Routing,State),
  {reply,OutFrames,Req,State#state{buffer=NewBuffer,routing=NewRouting}};
websocket_handle(Data, Req, State) ->
  erlang:error(unsupported,[Data,Req,State]),
  {ok, Req, State}.

websocket_info(erwa_stop, Req, State) ->
  {stop,Req,State};
websocket_info({erwa,Msg}, Req, #state{routing=Routing,ws_enc=WsEnc,enc=Enc}=State) when is_tuple(Msg)->
  Encode = fun(M) ->
             {WsEnc,wamper_protocol:serialize(M,Enc)}
           end,
  case erwa_routing:handle_info(Msg, Routing) of
    {ok, NewRouting} ->
      {ok,Req,State#state{routing=NewRouting}};
    {send, OutMsg, NewRouting} ->
      {reply,Encode(OutMsg),Req,State#state{routing=NewRouting}};
    {send_stop, OutMsg, NewRouting} ->
      self() ! erwa_stop,
      {reply,Encode(OutMsg),Req,State#state{routing=NewRouting}};
    {stop, NewRouting} ->
      {stop,Req,State#state{routing=NewRouting}}
  end;

websocket_info(_Data, Req, State) ->
  {ok,Req,State}.

terminate(_Reason, _Req, _State) ->
  ok.


handle_messages([],ToSend,Routing,_State) ->
  {ok,lists:reverse(ToSend),Routing};
handle_messages([Msg|Tail],ToSend,Routing,#state{ws_enc=WsEnc,enc=Enc}=State) ->
  Encode = fun(M) ->
             {WsEnc,wamper_protocol:serialize(M,Enc)}
           end,
  case erwa_routing:handle_message(Msg, Routing) of
    {ok, NewRouting} ->
      handle_messages(Tail,ToSend,NewRouting,State);
    {reply, OutMsg, NewRouting} ->
      handle_messages(Tail,[Encode(OutMsg)|ToSend],NewRouting,State);
    {reply_stop, OutMsg, NewRouting} ->
      self() ! erwa_stop,
      {ok,lists:reverse([Encode(OutMsg)|ToSend]),NewRouting};
    {stop, NewRouting} ->
      self() ! erwa_stop,
      {ok, lists:reverse(ToSend), NewRouting}
  end.



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




