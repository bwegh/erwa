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
-export([websocket_handle/3]).
-export([websocket_info/3]).

%cowboy 2.x
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

% both cowboy versions
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

%% ********************************************************
%% for cowboy 1.x
%% ********************************************************
init(_, Req, _ ) ->
	error_logger:info_report(["ERWA_IN_WS init.",
							  {req, Req}]),
	
	{upgrade, protocol, cowboy_websocket, Req, []}.

websocket_init( _Type, Req, _Opts) ->
	error_logger:info_report(["ERWA_IN_WS websocket_init.",
							  {req, Req}]),
	
	{ok,Protocols, Req1} = cowboy_req:parse_header(?SUBPROTHEADER, Req),
	lager:debug("protocols are ~p~n",[Protocols]),
	case find_supported_protocol(Protocols) of
		{Enc,WsEncoding,Header} ->
			Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,Header,Req1),
			Routing = erwa_routing:init(),
			{ok, Req2, #state{enc=Enc,ws_enc=WsEncoding,routing=Routing}};
		_ ->
			% unsupported
			{shutdown,Req1}
	end.

%% ********************************************************
%% for cowboy 2.x
%% ********************************************************
init(Req, _Opts) ->
	error_logger:info_report(["ERWA_IN_WS init.",
							  {req, Req},
							  {opts, _Opts}]),
	
	% need to check for the wamp.2.json or wamp.2.msgpack
	Protocols = cowboy_req:parse_header(?SUBPROTHEADER, Req),
	lager:debug("protocols are ~p~n",[Protocols]),
	case find_supported_protocol(Protocols) of
		{Enc,WsEncoding,Header} ->
			Req1  = cowboy_req:set_resp_header(?SUBPROTHEADER,Header,Req),
			Routing = erwa_routing:init(),
			
			error_logger:info_report(["ERWA_IN_WS init - reply.",
									  {req, Req1},
									  {state, [{enc, Enc},
											   {ws_enc, WsEncoding},
											   {routing, Routing}]}]),
			{cowboy_websocket, Req1, #state{enc=Enc,ws_enc=WsEncoding,routing=Routing}};
		_ ->
			% unsupported
			{shutdown,Req}
	end.

websocket_init(HandlerState) ->
	Routing = erwa_routing:init(),
	error_logger:info_report(["ERWA_IN_WS websocket_init.",
							  {handlerState, HandlerState},
							  {routing, Routing}]),
	{ok, HandlerState}.


%% ********************************************************
%% for cowboy 1.x
%% ********************************************************
websocket_handle({WsEnc, Data}, Req, #state{ws_enc=WsEnc,enc=Enc,buffer=Buffer,routing=Routing}=State) ->
	error_logger:info_report(["ERWA_IN_WS websocket_handle.",
							  {wsEnc, WsEnc},
							  {data, Data}]),
	
	{MList,NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
	{ok,OutFrames,NewRouting} = handle_messages(MList,[],Routing,State),
	
	error_logger:info_report(["ERWA_IN_WS websocket_handle - reply.",
							  {outFrames, OutFrames},
							  {state, [{buffer, NewBuffer},
									   {routing, NewRouting}]}]),
	
	{reply,OutFrames,Req,State#state{buffer=NewBuffer,routing=NewRouting}};
websocket_handle(Data, Req, State) ->
	erlang:error(unsupported,[Data,Req,State]),
	{ok, Req, State}.

%% ********************************************************
%% for cowboy 2.x
%% ********************************************************
websocket_handle({WsEnc, Data}, #state{ws_enc=WsEnc,enc=Enc,buffer=Buffer,routing=Routing}=State) ->
	error_logger:info_report(["ERWA_IN_WS websocket_handle.",
							  {wsEnc, WsEnc},
							  {data, Data}]),
	
	{MList,NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
	{ok,OutFrames,NewRouting} = handle_messages(MList,[],Routing,State),
	
	error_logger:info_report(["ERWA_IN_WS websocket_handle - reply.",
							  {outFrames, OutFrames},
							  {state, [{buffer, NewBuffer},
									   {routing, NewRouting}]}]),
	{reply, OutFrames, State#state{buffer=NewBuffer,routing=NewRouting}};
websocket_handle(Data, State) ->
	erlang:error(unsupported,[Data, State]),
	{ok, State}.

%% ********************************************************
%% for cowboy 1.x
%% ********************************************************
websocket_info(erwa_stop, Req, State) ->
	{stop, Req, State};
websocket_info({erwa,Msg}, Req, #state{routing=Routing,ws_enc=WsEnc,enc=Enc}=State) when is_tuple(Msg)->
	Encode = fun(M) ->
					 {WsEnc,wamper_protocol:serialize(M,Enc)}
			 end,
	case erwa_routing:handle_info(Msg, Routing) of
		{ok, NewRouting} ->
			{ok, Req, State#state{routing=NewRouting}};
		{send, OutMsg, NewRouting} ->
			{reply, Encode(OutMsg), Req, State#state{routing=NewRouting}};
		{send_stop, OutMsg, NewRouting} ->
			self() ! erwa_stop,
			{reply, Encode(OutMsg), Req, State#state{routing=NewRouting}};
		{stop, NewRouting} ->
			{stop, Req, State#state{routing=NewRouting}}
	end;
websocket_info(_Data, Req, State) ->
	{ok,Req,State}.

%% ********************************************************
%% for cowboy 2.x
%% ********************************************************
websocket_info(erwa_stop, State) ->
	{stop, State};
websocket_info({erwa,Msg}, #state{routing=Routing,ws_enc=WsEnc,enc=Enc}=State) when is_tuple(Msg)->
	error_logger:info_report(["ERWA_IN_WS websocket_into.",
							  {msg, Msg}]),
	
	Encode = fun(M) ->
					 {WsEnc,wamper_protocol:serialize(M,Enc)}
			 end,
	case erwa_routing:handle_info(Msg, Routing) of
		{ok, NewRouting} ->
			{ok, State#state{routing=NewRouting}};
		{send, OutMsg, NewRouting} ->
			{reply, Encode(OutMsg), State#state{routing=NewRouting}};
		{send_stop, OutMsg, NewRouting} ->
			self() ! erwa_stop,
			{reply, Encode(OutMsg), State#state{routing=NewRouting}};
		{stop, NewRouting} ->
			{stop, State#state{routing=NewRouting}}
	end;
websocket_info(_Data, State) ->
	{ok, State}.

%% ********************************************************
%% for all cowboy versions
%% ********************************************************
terminate(Reason, Req, State) ->
	error_logger:error_report(["ERWA_IN_WS has terminated.",
							   {reason, Reason},
							   {req, Req},
							   {state, State}]),
	ok.

%% ********************************************************
%% Internal function
%% ********************************************************
handle_messages([],ToSend,Routing,_State) ->
	{ok,lists:reverse(ToSend),Routing};
handle_messages([Msg|Tail],ToSend,Routing,#state{ws_enc=WsEnc,enc=Enc}=State) ->
	error_logger:info_report(["ERWA_IN_WS handle_messages - 1",
							  {msg, Msg},
							  {toSend, ToSend},
							  {routing, Routing},
							  {state, [{ws_enc, WsEnc},
									   {enc, Enc}]}]),
	
	Encode = fun(M) ->
					 {WsEnc,wamper_protocol:serialize(M,Enc)}
			 end,
	
	error_logger:info_report(["ERWA_IN_WS handle_messages - 2",
							  {encode, Encode}]),
	
	case erwa_routing:handle_message(Msg, Routing) of
		{ok, NewRouting} ->
			error_logger:info_report(["ERWA_IN_WS handle_messages - reply_1",
									  {newRouting, NewRouting}]),
			
			handle_messages(Tail,ToSend,NewRouting,State);
		{reply, OutMsg, NewRouting} ->
			error_logger:info_report(["ERWA_IN_WS handle_messages - reply_2",
									  {reply, [{outMsg, OutMsg},
											   {newRouting, NewRouting}]}]),
			
			handle_messages(Tail,[Encode(OutMsg)|ToSend],NewRouting,State);
		{reply_stop, OutMsg, NewRouting} ->
			error_logger:info_report(["ERWA_IN_WS handle_messages - reply_3",
									  {reply_stop, [{outMsg, OutMsg},
													{newRouting, NewRouting}]}]),
			
			self() ! erwa_stop,
			{ok,lists:reverse([Encode(OutMsg)|ToSend]),NewRouting};
		{stop, NewRouting} ->
			error_logger:info_report(["ERWA_IN_WS handle_messages - reply_4",
									  {stop, [{newRouting, NewRouting}]}]),
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




