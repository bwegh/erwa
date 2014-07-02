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

-module(erwa_protocol).

-export([forward_messages/2]).
-export([deserialize/2]).
-export([serialize/2]).
-export([to_wamp/1]).
-export([to_erl/1]).

-export([is_valid_uri/1]).
-export([is_valid_id/1]).
-export([is_valid_dict/1]).


deserialize(Buffer,Encoding) ->
  deserialize(Buffer,[],Encoding).

-spec deserialize(Buffer :: binary(), Messages :: list(), Encoding :: atom() ) -> {[Message :: term()], NewBuffer :: binary()}.

deserialize(Buffer,Messages,msgpack) ->
  case msgpack:unpack_stream(Buffer,[{format,jsx}]) of
    {error,incomplete} ->
      {to_erl_reverse(Messages),Buffer};
     {Msg,NewBuffer} ->
      deserialize(NewBuffer,[Msg|Messages],msgpack)
  end;
deserialize(Buffer,Messages,json) ->
  %% is it possible to check the data here ?
  %% length and stuff, yet should not be needed
  {[to_erl(jsx:decode(Buffer))|Messages],<<"">>};
deserialize(<<Len:32/unsigned-integer-big,Data/binary>>  = Buffer,Messages,raw_msgpack) ->
  case byte_size(Data) >= Len of
    true ->
      <<Enc:Len/binary,NewBuffer/binary>> = Data,
      {ok,Msg} = msgpack:unpack(Enc,[{format,jsx}]),
      deserialize(NewBuffer,[Msg|Messages],raw_msgpack);
    false ->
      {to_erl_reverse(Messages),Buffer}
  end;
deserialize(<<Len:32/unsigned-integer-big,Data/binary>>  = Buffer,Messages,raw_json) ->
  case byte_size(Data) >= Len of
    true ->
      <<Enc:Len/binary,NewBuffer/binary>> = Data,
      deserialize(NewBuffer,[jsx:decode(Enc)|Messages],raw_json);
    false ->
      {to_erl_reverse(Messages),Buffer}
  end;
deserialize(Buffer,Messages,_) ->
  {to_erl_reverse(Messages),Buffer}.



serialize(Erwa,Enc) when is_tuple(Erwa) ->
  WAMP = to_wamp(Erwa),
  serialize(WAMP,Enc);
serialize(Msg,msgpack)  ->
  msgpack:pack(Msg,[{format,jsx}]);
serialize(Msg,json)  ->
  jsx:encode(Msg);
serialize(Message,raw_msgpack) ->
  Enc = msgpack:pack(Message,[jsx]),
  Len = byte_size(Enc),
  <<Len:32/unsigned-integer-big,Enc/binary>>;
serialize(Message,raw_json) ->
  Enc = jsx:encode(Message),
  Len = byte_size(Enc),
  <<Len:32/unsigned-integer-big,Enc/binary>>.

-spec forward_messages(Messages :: list(), Router :: pid() | undefined) -> {ok,Router :: pid() | undefined} | {error,not_found}.
forward_messages([],Router) ->
  {ok,Router};
forward_messages([{hello,Realm,_}|_]=Messages,undefined) ->
  case erwa_realms:get_router(Realm) of
    {ok,Pid} ->
      forward_messages(Messages,Pid);
    {error,not_found} ->
      self() ! {erwa,{abort,[{}],no_such_realm}},
      self() ! {erwa, shutdown},
      {error,undefined}
  end;
forward_messages([Msg|T],Router) when is_pid(Router) ->
  ok = erwa_router:handle_wamp(Router,Msg),
  forward_messages(T,Router);
forward_messages(_,undefined)  ->
  self() ! {erwa,{abort,[{}],no_such_realm}},
  self() ! {erwa, shutdown},
  {error,undefined}.




to_erl_reverse(List)->
  to_erl_reverse(List,[]).

to_erl_reverse([],List) ->
  List;
to_erl_reverse([H|T],Messages) ->
  to_erl_reverse(T,[to_erl(H)|Messages]).

is_valid_uri(Uri) when is_binary(Uri) -> true;
is_valid_uri(_) -> false.

is_valid_id(Id) when is_integer(Id), Id >= 0, Id < 9007199254740992 -> true;
is_valid_id(_) -> false.

is_valid_dict(Dict) when is_list(Dict) -> true;
is_valid_dict(_) -> false.

is_valid_list(List) when is_list(List) -> true;
is_valid_list(_) -> false.


-define(HELLO,1).
-define(WELCOME,2).
-define(ABORT,3).
-define(CHALLENGE,4).
-define(AUTHENTICATE,5).
-define(GOODBYE,6).
-define(HEARTBEAT,7).
-define(ERROR,8).

-define(PUBLISH,16).
-define(PUBLISHED,17).

-define(SUBSCRIBE,32).
-define(SUBSCRIBED,33).
-define(UNSUBSCRIBE,34).
-define(UNSUBSCRIBED,35).
-define(EVENT,36).

-define(CALL,48).
-define(CANCEL,49).
-define(RESULT,50).

-define(REGISTER,64).
-define(REGISTERED,65).
-define(UNREGISTER,66).
-define(UNREGISTERED,67).
-define(INVOCATION,68).
-define(INTERRUPT,69).
-define(YIELD,70).

-define(ERROR_NOT_AUTHORIZED,<<"wamp.error.not_authorized">>).
-define(ERROR_NO_SUCH_REALM,<<"wamp.error.no_such_realm">>).
-define(ERROR_SHUTDOWN,<<"wamp.error.system_shutdown">>).
-define(ERROR_CLOSE_REALM,<<"wamp.error.close_realm">>).
-define(ERROR_AND_OUT,<<"wamp.error.goodbye_and_out">>).
-define(ERROR_NO_SUCH_PROCEDURE,<<"wamp.error.no_such_procedure">>).
-define(ERROR_NO_SUCH_SUBSCRIPTION,<<"wamp.error.no_such_subscription">>).
-define(ERROR_NO_SUCH_REGISTRATION,<<"wamp.error.no_such_registration">>).
-define(ERROR_INVALID_ARGUMENT,<<"wamp.error.invalid_argument">>).
-define(ERROR_INVALID_TOPIC,<<"wamp.error.invalid_topic">>).
-define(ERROR_PROCEDURE_ALREADY_EXISTS,<<"wamp.error.procedure_already_exists">>).

to_erl([?HELLO,Realm,Details]) ->
  true = is_valid_uri(Realm),
  true = is_valid_dict(Details),
  {hello,Realm,Details};

to_erl([?WELCOME,SessionId,Details]) ->
  true = is_valid_id(SessionId),
  true = is_valid_dict(Details),
  {welcome,SessionId,Details};

to_erl([?ABORT,Details,Reason]) ->
  true = is_valid_dict(Details),
  true = is_valid_uri(Reason),
  {abort,Details,Reason};

to_erl([?GOODBYE,Details,?ERROR_NOT_AUTHORIZED]) ->
  to_erl([?GOODBYE,Details,not_authorized]);
to_erl([?GOODBYE,Details,?ERROR_NO_SUCH_REALM]) ->
  to_erl([?GOODBYE,Details,no_such_realm]);
to_erl([?GOODBYE,Details,?ERROR_SHUTDOWN]) ->
  to_erl([?GOODBYE,Details,system_shutdown]);
to_erl([?GOODBYE,Details,?ERROR_CLOSE_REALM]) ->
  to_erl([?GOODBYE,Details,close_realm]);
to_erl([?GOODBYE,Details,?ERROR_AND_OUT]) ->
  to_erl([?GOODBYE,Details,goodbye_and_out]);
to_erl([?GOODBYE,Details,Reason]) ->
  true = is_valid_dict(Details),
  true = is_valid_uri(Reason) or is_atom(Reason),
  {goodbye,Details,Reason};

to_erl([?ERROR,RequestType,RequestId,Details,Error]) ->
  to_erl([?ERROR,RequestType,RequestId,Details,Error,undefined,undefined]);
to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments]) ->
  to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments,undefined]);

to_erl([?ERROR,?CALL,RequestId,Details,?ERROR_NO_SUCH_PROCEDURE,Arguments,ArgumentsKw]) ->
  to_erl([?ERROR,?CALL,RequestId,Details,no_such_procedure,Arguments,ArgumentsKw]);

to_erl([?ERROR,?CALL,RequestId,Details,Error,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Details),
  true = is_valid_uri(Error) or is_atom(Error),
  true = is_valid_list(Arguments) or is_atom(Arguments),
  true = is_valid_dict(ArgumentsKw) or is_atom(ArgumentsKw),
  {error,call,RequestId,Details,Error,Arguments,ArgumentsKw};

to_erl([?PUBLISH,RequestId,Options,Topic]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,undefined,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  true = is_valid_list(Arguments) or is_atom(Arguments),
  true = is_valid_dict(ArgumentsKw) or is_atom(ArgumentsKw),
  {publish,RequestId,Options,Topic,Arguments,ArgumentsKw};

to_erl([?PUBLISHED,RequestId,PublicationId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(PublicationId),
  {published,RequestId,PublicationId};

to_erl([?SUBSCRIBE,RequestId,Options,Topic]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  {subscribe,RequestId,Options,Topic};

to_erl([?SUBSCRIBED,RequestId,SubscriptionId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(SubscriptionId),
  {subscribed,RequestId,SubscriptionId};

to_erl([?UNSUBSCRIBE,RequestId,SubscriptionId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(SubscriptionId),
  {unsubscribe,RequestId,SubscriptionId};

to_erl([?UNSUBSCRIBED,RequestId]) ->
  true = is_valid_id(RequestId),
  {unsubscribed,RequestId};

to_erl([?EVENT,SubscriptionId,PublicationId,Details]) ->
  to_erl([?EVENT,SubscriptionId,PublicationId,Details,undefined,undefined]);
to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments]) ->
  to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments,undefined]);
to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw]) ->
  true = is_valid_id(SubscriptionId),
  true = is_valid_id(PublicationId),
  true = is_valid_dict(Details),
  true = is_valid_list(Arguments) or is_atom(Arguments),
  true = is_valid_dict(ArgumentsKw) or is_atom(ArgumentsKw),
  {event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw};

to_erl([?CALL,RequestId,Options,Procedure]) ->
  to_erl([?CALL,RequestId,Options,Procedure,undefined,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments]) ->
  to_erl([?CALL,RequestId,Options,Procedure,Arguments,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  true = is_valid_list(Arguments) or is_atom(Arguments),
  true = is_valid_dict(ArgumentsKw) or is_atom(ArgumentsKw),
  {call,RequestId,Options,Procedure,Arguments,ArgumentsKw};

to_erl([?RESULT,RequestId,Details]) ->
  to_erl([?RESULT,RequestId,Details,undefined,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments]) ->
  to_erl([?RESULT,RequestId,Details,Arguments,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Details),
  true = is_valid_list(Arguments) or is_atom(Arguments),
  true = is_valid_dict(ArgumentsKw) or is_atom(ArgumentsKw),
  {result,RequestId,Details,Arguments,ArgumentsKw};

to_erl([?REGISTER,RequestId,Options,Procedure]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  {register,RequestId,Options,Procedure};

to_erl([?REGISTERED,RequestId,RegistrationId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(RegistrationId),
  {registered,RequestId,RegistrationId};

to_erl([?UNREGISTER,RequestId,RegistrationId]) ->
  {unregister,RequestId,RegistrationId};

to_erl([?UNREGISTERED,RequestId]) ->
  true = is_valid_id(RequestId),
  {unregistered,RequestId};

to_erl([?INVOCATION,RequestId, RegistrationId, Details]) ->
  to_erl([?INVOCATION,RequestId, RegistrationId, Details, undefined, undefined]);
to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments]) ->
  to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments, undefined]);
to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments, ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(RegistrationId),
  true = is_valid_dict(Details),
  true = is_valid_list(Arguments) or is_atom(Arguments),
  true = is_valid_dict(ArgumentsKw) or is_atom(ArgumentsKw),
  {invocation,RequestId, RegistrationId, Details, Arguments, ArgumentsKw};

to_erl([?YIELD, RequestId, Options]) ->
  to_erl([?YIELD, RequestId, Options, undefined, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments]) ->
  to_erl([?YIELD, RequestId, Options, Arguments, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_list(Arguments) or is_atom(Arguments),
  true = is_valid_dict(ArgumentsKw) or is_atom(ArgumentsKw),
  {yield,RequestId,Options, Arguments,ArgumentsKw}.






to_wamp({hello,Realm,Details}) ->
  [?HELLO,Realm,Details];

to_wamp({welcome,SessionId,Details}) ->
  [?WELCOME,SessionId,Details];

to_wamp({abort,Details,not_authorized}) ->
  [?ABORT,Details,?ERROR_NOT_AUTHORIZED];
to_wamp({abort,Details,no_such_realm}) ->
  [?ABORT,Details,?ERROR_NO_SUCH_REALM];
to_wamp({abort,Details,close_realm}) ->
  [?ABORT,Details,?ERROR_CLOSE_REALM];
to_wamp({abort,Details,system_shutdown}) ->
  [?ABORT,Details,?ERROR_SHUTDOWN];

to_wamp({goodbye,Details,close_realm}) ->
  [?GOODBYE,Details,?ERROR_CLOSE_REALM];
to_wamp({goodbye,Details,system_shutdown}) ->
  [?GOODBYE,Details,?ERROR_SHUTDOWN];
to_wamp({goodbye,Details,invalid_argument}) ->
  [?GOODBYE,Details,?ERROR_INVALID_ARGUMENT];
to_wamp({goodbye,Details,goodbye_and_out}) ->
  [?GOODBYE,Details,?ERROR_AND_OUT];

to_wamp({error,unsubscribe,RequestId,Details,no_such_subscription}) ->
  [?ERROR,?SUBSCRIBE,RequestId,Details,?ERROR_NO_SUCH_SUBSCRIPTION];
to_wamp({error,register,RequestId,Details,procedure_already_exists}) ->
  [?ERROR,?SUBSCRIBE,RequestId,Details,?ERROR_PROCEDURE_ALREADY_EXISTS];
to_wamp({error,unregister,RequestId,Details,no_such_registration}) ->
  [?ERROR,?SUBSCRIBE,RequestId,Details,?ERROR_NO_SUCH_REGISTRATION];
to_wamp({error,call,RequestId,Details,no_such_procedure,undefined,undefined}) ->
  [?ERROR,?CALL,RequestId,Details,?ERROR_NO_SUCH_PROCEDURE];
to_wamp({error,call,RequestId,Details,no_such_procedure,Arguments,undefined}) ->
  [?ERROR,?CALL,RequestId,Details,?ERROR_NO_SUCH_PROCEDURE,Arguments];
to_wamp({error,call,RequestId,Details,no_such_procedure,Arguments,ArgumentsKw}) ->
  [?ERROR,?CALL,RequestId,Details,?ERROR_NO_SUCH_PROCEDURE,Arguments,ArgumentsKw];

to_wamp({publish,RequestId,Options,Topic,undefined,undefined}) ->
  [?PUBLISH,RequestId,Options,Topic];
to_wamp({publish,RequestId,Options,Topic,Arguments,undefined}) ->
  [?PUBLISH,RequestId,Options,Topic,Arguments];
to_wamp({publish,RequestId,Options,Topic,Arguments,ArgumentsKw}) ->
  [?PUBLISH,RequestId,Options,Topic,Arguments,ArgumentsKw];

to_wamp({published,RequestId,PublicationId}) ->
  [?PUBLISHED,RequestId,PublicationId];

to_wamp({subscribe,RequestId,Options,Topic}) ->
  [?SUBSCRIBE,RequestId,Options,Topic];

to_wamp({subscribed,RequestId,SubscriptionId}) ->
  [?SUBSCRIBED,RequestId,SubscriptionId];

to_wamp({unsubscribe,RequestId,SubscriptionId}) ->
  [?UNSUBSCRIBE,RequestId,SubscriptionId];

to_wamp({unsubscribed,RequestId}) ->
  [?UNSUBSCRIBED,RequestId];

to_wamp({event,SubscriptionId,PublicationId,Details,undefined,undefined}) ->
  [?EVENT,SubscriptionId,PublicationId,Details];
to_wamp({event,SubscriptionId,PublicationId,Details,Arguments,undefined}) ->
  [?EVENT,SubscriptionId,PublicationId,Details,Arguments];
to_wamp({event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw}) ->
  [?EVENT,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw];

to_wamp({call,RequestId,Options,Procedure,undefined,undefined}) ->
  [?CALL,RequestId,Options,Procedure];
to_wamp({call,RequestId,Options,Procedure,Arguments,undefined}) ->
  [?CALL,RequestId,Options,Procedure,Arguments];
to_wamp({call,RequestId,Options,Procedure,Arguments,ArgumentsKw}) ->
  [?CALL,RequestId,Options,Procedure,Arguments,ArgumentsKw];


to_wamp({result,RequestId,Details,undefined,undefined}) ->
  [?RESULT,RequestId,Details];
to_wamp({result,RequestId,Details,Arguments,undefined}) ->
  [?RESULT,RequestId,Details,Arguments];
to_wamp({result,RequestId,Details,Arguments,ArgumentsKw}) ->
  [?RESULT,RequestId,Details,Arguments,ArgumentsKw];

to_wamp({register,RequestId,Options,Procedure}) ->
  [?REGISTER,RequestId,Options,Procedure];

to_wamp({registered,RequestId,RegistrationId}) ->
  [?REGISTERED,RequestId,RegistrationId];

to_wamp({unregister,RequestId,RegistrationId}) ->
  [?UNREGISTER,RequestId,RegistrationId];

to_wamp({unregistered,RequestId}) ->
  [?UNREGISTERED,RequestId];

to_wamp({invocation,RequestId,RegistrationId,Details,undefined,undefined}) ->
  [?INVOCATION,RequestId,RegistrationId,Details];
to_wamp({invocation,RequestId,RegistrationId,Details,Arguments,undefined}) ->
  [?INVOCATION,RequestId,RegistrationId,Details,Arguments];
to_wamp({invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}) ->
  [?INVOCATION,RequestId,RegistrationId,Details,Arguments,ArgumentsKw];

to_wamp({yield,RequestId,Options,undefined,undefined}) ->
  [?YIELD,RequestId,Options];
to_wamp({yield,RequestId,Options,Arguments,undefined}) ->
  [?YIELD,RequestId,Options,Arguments];
to_wamp({yield,RequestId,Options,Arguments,ArgumentsKw}) ->
  [?YIELD,RequestId,Options,Arguments,ArgumentsKw];

to_wamp(noreply)  ->
  noreply;
to_wamp(shutdown)  ->
  shutdown.


-ifdef(TEST).

validation_test() ->
  true = is_valid_id(0),
  true = is_valid_id(9007199254740991),
  false = is_valid_id(9007199254740992),
  false = is_valid_id(-1),
  false = is_valid_id(0.1),

  true = is_valid_uri(<<"wamp.ws">>),

  true = is_valid_list([]),

  true = is_valid_dict([]),
  ok.


hello_test() ->
  M = [?HELLO,<<"realm1">>,[{}]],
  S = serialize(M,json),
  io:format("~p serialized to ~p~n",[M,S]),
  D = deserialize(S,json),
  io:format("~p deserialized to ~p~n",[S,D]),
  D = {[{hello,<<"realm1">>,[{}]}],<<"">>}.

roundtrip_test() ->

  Messages = [
              {hello,<<"realm1">>,[{}]}
              ],


  Serializer = fun(Message,Res) ->

                 Encodings = [json,msgpack,raw_json,raw_msgpack],

                 Check = fun(Enc,Bool) ->
                           EncMsg = serialize(Message,Enc),
                           io:format("serializing ~p to ~p when using ~p~n",[Message,EncMsg,Enc]),
                           DeEncMsg = deserialize(EncMsg,Enc) ,
                           io:format("  and deserialize it back to ~p~n",[DeEncMsg]),
                           case DeEncMsg of
                             {[Message],<<"">>} -> Bool;
                             _ -> false
                           end
                         end,
               Res and lists:foldl(Check,true,Encodings)
               end,

  true = lists:foldl(Serializer,true,Messages).



-endif.

