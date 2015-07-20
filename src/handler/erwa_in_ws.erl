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

-behaviour(cowboy_websocket).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% for websocket
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-define(TIMEOUT, 60000).

-define(SUBPROTHEADER, <<"sec-websocket-protocol">>).
-define(WSMSGPACK, <<"wamp.2.msgpack">>).
-define(WSJSON, <<"wamp.2.json">>).
-define(WSMSGPACK_BATCHED, <<"wamp.2.msgpack.batched">>).
-define(WSJSON_BATCHED, <<"wamp.2.json.batched">>).


-record(state, {
	enc = undefined,
	ws_enc = undefined,
	length = infitity,
	buffer = <<"">>,
	session = undefined
}).


init(Req, _Opts) ->
	% need to check for the wamp.2.json or wamp.2.msgpack
	Protocols = cowboy_req:parse_header(?SUBPROTHEADER, Req),
	case find_supported_protocol(Protocols) of
		{Enc, WsEncoding, Header} ->
			Req1 = cowboy_req:set_resp_header(?SUBPROTHEADER, Header, Req),
			Peer = cowboy_req:peer(Req1),
			Session = erwa_session:create(),
			Session1 = erwa_session:set_peer(Peer, Session),
			Session2 = erwa_session:set_source(websocket, Session1),
			{cowboy_websocket, Req1, #state{enc = Enc, ws_enc = WsEncoding, session = Session2}};
		_ ->
			% unsupported
			{shutdown, Req}
	end.


websocket_handle({WsEnc, Data}, Req, #state{ws_enc = WsEnc, enc = Enc, buffer = Buffer, session = Session} = State) ->
	{MList, NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>, Enc),
	{ok, OutFrames, NewSession} = handle_messages(MList, [], Session, State),
	{reply, OutFrames, Req, State#state{buffer = NewBuffer, session = NewSession}};
websocket_handle(Data, Req, State) ->
	erlang:error(unsupported, [Data, Req, State]),
	{ok, Req, State}.

websocket_info(erwa_stop, Req, State) ->
	{stop, Req, State};
websocket_info({erwa, Msg}, Req, #state{session = Session, ws_enc = WsEnc, enc = Enc} = State) when is_tuple(Msg) ->
	Encode = fun(M) -> {WsEnc, wamper_protocol:serialize(M, Enc)} end,
	case erwa_session:handle_info(Msg, Session) of
		{ok, NewSession} ->
			{ok, Req, State#state{session = NewSession}};
		{send, OutMsg, NewSession} ->
			{reply, Encode(OutMsg), Req, State#state{session = NewSession}};
		{send_stop, OutMsg, NewSession} ->
			self() ! erwa_stop,
			{reply, Encode(OutMsg), Req, State#state{session = NewSession}};
		{stop, NewSession} ->
			{stop, Req, State#state{session = NewSession}}
	end;
websocket_info(_Data, Req, State) ->
	{ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.


%% @private
handle_messages([], ToSend, Session, _State) ->
	{ok, lists:reverse(ToSend), Session};
handle_messages([Msg | Tail], ToSend, Session, #state{ws_enc = WsEnc, enc = Enc} = State) ->
	Encode = fun(M) -> {WsEnc, wamper_protocol:serialize(M, Enc)} end,
	case erwa_session:handle_message(Msg, Session) of
		{ok, NewSession} ->
			handle_messages(Tail, ToSend, NewSession, State);
		{reply, OutMsg, NewSession} ->
			handle_messages(Tail, [Encode(OutMsg) | ToSend], NewSession, State);
		{reply_stop, OutMsg, NewSession} ->
			self() ! erwa_stop,
			{ok, lists:reverse([Encode(OutMsg) | ToSend]), NewSession};
		{stop, NewSession} ->
			self() ! erwa_stop,
			{ok, lists:reverse(ToSend), NewSession}
	end.

%% @private
-spec find_supported_protocol([binary()]) ->
	atom() | {json|json_batched|msgpack|msgpack_batched, text|binary, binary()}.
find_supported_protocol([]) -> none;
find_supported_protocol([?WSJSON | _T]) -> {json, text, ?WSJSON};
find_supported_protocol([?WSJSON_BATCHED | _T]) -> {json_batched, text, ?WSJSON_BATCHED};
find_supported_protocol([?WSMSGPACK | _T]) -> {msgpack, binary, ?WSMSGPACK};
find_supported_protocol([?WSMSGPACK_BATCHED | _T]) -> {msgpack_batched, binary, ?WSMSGPACK_BATCHED};
find_supported_protocol([_ | T]) -> find_supported_protocol(T).