%%
%% Copyright (c) 2014-2015 Bas Wegh
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
-module(erwa_protocol).

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
deserialize(Buffer,Messages,msgpack_batched) ->
  deserialize(Buffer,Messages,raw_msgpack);
deserialize(Buffer,Messages,json) ->
  %% is it possible to check the data here ?
  %% length and stuff, yet should not be needed
  {[to_erl(jsx:decode(Buffer))|Messages],<<"">>};
deserialize(Buffer,_Messages,json_batched) ->
  Wamps = binary:split(Buffer,[<<24>>],[global,trim]),
  {to_erl_reverse(lists:foldl(fun(M,List) -> [jsx:decode(M)|List] end,[],Wamps)),<<"">>};
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
serialize(Msg,msgpack_batched) ->
  serialize(Msg,raw_msgpack);
serialize(Msg,json)  ->
  jsx:encode(Msg);
serialize(Msg,json_batched) ->
  Enc = jsx:encode(Msg),
  <<Enc/binary,24>>;
serialize(Message,raw_msgpack) ->
  Enc = msgpack:pack(Message,[jsx]),
  Len = byte_size(Enc),
  <<Len:32/unsigned-integer-big,Enc/binary>>;
serialize(Message,raw_json) ->
  Enc = jsx:encode(Message),
  Len = byte_size(Enc),
  <<Len:32/unsigned-integer-big,Enc/binary>>.





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

is_valid_arguments(List) when is_list(List) -> true;
is_valid_arguments(undefined)  -> true;
is_valid_arguments(_)  -> false.

is_valid_argumentskw(List) when is_list(List) -> true;
is_valid_argumentskw(undefined)  -> true;
is_valid_argumentskw(_)  -> false.

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



-define(ERROR_AND_OUT,<<"wamp.error.goodbye_and_out">>).
-define(ERROR_AUTHORIZATION_FAILED,<<"wamp.error.authorization_failed">>).
-define(ERROR_CLOSE_REALM,<<"wamp.error.close_realm">>).
-define(ERROR_INVALID_ARGUMENT,<<"wamp.error.invalid_argument">>).
-define(ERROR_INVALID_URI,<<"wamp.error.invalid_uri">>).
-define(ERROR_NO_SUCH_PROCEDURE,<<"wamp.error.no_such_procedure">>).
-define(ERROR_NO_SUCH_REALM,<<"wamp.error.no_such_realm">>).
-define(ERROR_NO_SUCH_REGISTRATION,<<"wamp.error.no_such_registration">>).
-define(ERROR_NO_SUCH_ROLE,<<"wamp.error.no_such_role">>).
-define(ERROR_NO_SUCH_SUBSCRIPTION,<<"wamp.error.no_such_subscription">>).
-define(ERROR_NOT_AUTHORIZED,<<"wamp.error.not_authorized">>).
-define(ERROR_PROCEDURE_ALREADY_EXISTS,<<"wamp.error.procedure_already_exists">>).
-define(ERROR_SHUTDOWN,<<"wamp.error.system_shutdown">>).



to_erl([?HELLO,Realm,Details]) ->
  true = is_valid_uri(Realm),
  true = is_valid_dict(Details),
  {hello,Realm,dict_to_erl(Details)};

to_erl([?WELCOME,SessionId,Details]) ->
  true = is_valid_id(SessionId),
  true = is_valid_dict(Details),
  {welcome,SessionId,dict_to_erl(Details)};

to_erl([?ABORT,Details,Reason]) ->
  true = is_valid_dict(Details),
  true = is_valid_uri(Reason),
  {abort,dict_to_erl(Details),Reason};


to_erl([?CHALLENGE,<<"wampcra">>,Extra]) ->
  to_erl([?CHALLENGE,wampcra,Extra]);
to_erl([?CHALLENGE,AuthMethod,Extra]) ->
  true = is_valid_dict(Extra),
  {challenge,AuthMethod,dict_to_erl(Extra)};

to_erl([?AUTHENTICATE,Signature,Extra]) ->
  true = is_valid_dict(Extra),
  {authenticate,Signature,dict_to_erl(Extra)};

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
  {goodbye,dict_to_erl(Details),Reason};

to_erl([?HEARTBEAT,IncomingSeq,OutgoingSeq,_Discard]) ->
  to_erl([?HEARTBEAT,IncomingSeq,OutgoingSeq]);
to_erl([?HEARTBEAT,IncomingSeq,OutgoingSeq]) ->
  {heartbeat,IncomingSeq,OutgoingSeq};

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
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {error,call,RequestId,dict_to_erl(Details),Error,Arguments,ArgumentsKw};

to_erl([?PUBLISH,RequestId,Options,Topic]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,undefined,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {publish,RequestId,dict_to_erl(Options),Topic,Arguments,ArgumentsKw};

to_erl([?PUBLISHED,RequestId,PublicationId]) ->
  true = is_valid_id(RequestId),
  true = is_valid_id(PublicationId),
  {published,RequestId,PublicationId};

to_erl([?SUBSCRIBE,RequestId,Options,Topic]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Topic),
  {subscribe,RequestId,dict_to_erl(Options),Topic};

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
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {event,SubscriptionId,PublicationId,dict_to_erl(Details),Arguments,ArgumentsKw};

to_erl([?CALL,RequestId,Options,Procedure]) ->
  to_erl([?CALL,RequestId,Options,Procedure,undefined,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments]) ->
  to_erl([?CALL,RequestId,Options,Procedure,Arguments,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {call,RequestId,dict_to_erl(Options),Procedure,Arguments,ArgumentsKw};

to_erl([?CANCEL,RequestId,Options]) ->
  true = is_valid_dict(Options),
  {cancel,RequestId,dict_to_erl(Options)};

to_erl([?RESULT,RequestId,Details]) ->
  to_erl([?RESULT,RequestId,Details,undefined,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments]) ->
  to_erl([?RESULT,RequestId,Details,Arguments,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments,ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Details),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {result,RequestId,dict_to_erl(Details),Arguments,ArgumentsKw};

to_erl([?REGISTER,RequestId,Options,Procedure]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_uri(Procedure),
  {register,RequestId,dict_to_erl(Options),Procedure};

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
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {invocation,RequestId, RegistrationId, dict_to_erl(Details), Arguments, ArgumentsKw};

to_erl([?INTERRUPT,RequestId,Options]) ->
  true = is_valid_dict(Options),
  {interrupt,RequestId,dict_to_erl(Options)};

to_erl([?YIELD, RequestId, Options]) ->
  to_erl([?YIELD, RequestId, Options, undefined, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments]) ->
  to_erl([?YIELD, RequestId, Options, Arguments, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
  true = is_valid_id(RequestId),
  true = is_valid_dict(Options),
  true = is_valid_arguments(Arguments),
  true = is_valid_argumentskw(ArgumentsKw),
  {yield,RequestId,dict_to_erl(Options), Arguments,ArgumentsKw}.






to_wamp({hello,Realm,Details}) ->
  [?HELLO,Realm,dict_to_wamp(Details)];

to_wamp({challenge,wampcra,Extra}) ->
   to_wamp({challenge,<<"wampcra">>,Extra});
to_wamp({challenge,AuthMethod,Extra}) ->
  [?CHALLENGE,AuthMethod,dict_to_wamp(Extra)];

to_wamp({authenticate,Signature,Extra}) ->
  [?AUTHENTICATE,Signature,dict_to_wamp(Extra)];

to_wamp({welcome,SessionId,Details}) ->
  [?WELCOME,SessionId,dict_to_wamp(Details)];

to_wamp({heartbeat,IncomingSeq,OutgoingSeq}) ->
  [?HEARTBEAT,IncomingSeq,OutgoingSeq];

to_wamp({abort,Details,not_authorized}) ->
  to_wamp({abort,Details,?ERROR_NOT_AUTHORIZED});
to_wamp({abort,Details,no_such_realm}) ->
  to_wamp({abort,Details,?ERROR_NO_SUCH_REALM});
to_wamp({abort,Details,close_realm}) ->
  to_wamp({abort,Details,?ERROR_CLOSE_REALM});
to_wamp({abort,Details,system_shutdown}) ->
  to_wamp({abort,Details,?ERROR_SHUTDOWN});
to_wamp({abort,Details,Reason}) ->
  [?ABORT,dict_to_wamp(Details),Reason];


to_wamp({goodbye,Details,close_realm}) ->
  to_wamp({goodbye,Details,?ERROR_CLOSE_REALM});
to_wamp({goodbye,Details,system_shutdown}) ->
  to_wamp({goodbye,Details,?ERROR_SHUTDOWN});
to_wamp({goodbye,Details,invalid_argument}) ->
  to_wamp({goodbye,Details,?ERROR_INVALID_ARGUMENT});
to_wamp({goodbye,Details,goodbye_and_out}) ->
  to_wamp({goodbye,Details,?ERROR_AND_OUT});
to_wamp({goodbye,Details,Reason}) ->
  [?GOODBYE,dict_to_wamp(Details),Reason];


to_wamp({error,unsubscribe,RequestId,Details,no_such_subscription,Arguments,ArgumentsKw}) ->
  to_wamp({error,?UNSUBSCRIBE,RequestId,Details,?ERROR_NO_SUCH_SUBSCRIPTION,Arguments,ArgumentsKw});
to_wamp({error,register,RequestId,Details,procedure_already_exists,Arguments,ArgumentsKw}) ->
  to_wamp({error,?REGISTER,RequestId,Details,?ERROR_PROCEDURE_ALREADY_EXISTS,Arguments,ArgumentsKw});
to_wamp({error,unregister,RequestId,Details,no_such_registration,Arguments,ArgumentsKw}) ->
  to_wamp({error,?UNREGISTER,RequestId,Details,?ERROR_NO_SUCH_REGISTRATION,Arguments,ArgumentsKw});

to_wamp({error,call,RequestId,Details,no_such_procedure,Arguments,ArgumentsKw}) ->
  to_wamp({error,?CALL,RequestId,Details,?ERROR_NO_SUCH_PROCEDURE,Arguments,ArgumentsKw});
to_wamp({error,call,RequestId,Details,invalid_argument,Arguments,ArgumentsKw}) ->
  to_wamp({error,?CALL,RequestId,Details,?ERROR_INVALID_ARGUMENT,Arguments,ArgumentsKw});

to_wamp({error,invocation,RequestId,Details,invalid_argument,Arguments,ArgumentsKw}) ->
  to_wamp({error,?INVOCATION,RequestId,Details,?ERROR_INVALID_ARGUMENT,Arguments,ArgumentsKw});


to_wamp({error,Origin,RequestId,Details,Reason,undefined,undefined}) ->
  [?ERROR,Origin,RequestId,dict_to_wamp(Details),Reason];
to_wamp({error,Origin,RequestId,Details,Reason,Arguments,undefined}) ->
  [?ERROR,Origin,RequestId,dict_to_wamp(Details),Reason,Arguments];
to_wamp({error,Origin,RequestId,Details,Reason,Arguments,ArgumentsKw}) ->
  [?ERROR,Origin,RequestId,dict_to_wamp(Details),Reason,Arguments,ArgumentsKw];

to_wamp({publish,RequestId,Options,Topic,undefined,undefined}) ->
  [?PUBLISH,RequestId,dict_to_wamp(Options),Topic];
to_wamp({publish,RequestId,Options,Topic,Arguments,undefined}) ->
  [?PUBLISH,RequestId,dict_to_wamp(Options),Topic,Arguments];
to_wamp({publish,RequestId,Options,Topic,Arguments,ArgumentsKw}) ->
  [?PUBLISH,RequestId,dict_to_wamp(Options),Topic,Arguments,ArgumentsKw];

to_wamp({published,RequestId,PublicationId}) ->
  [?PUBLISHED,RequestId,PublicationId];

to_wamp({subscribe,RequestId,Options,Topic}) ->
  [?SUBSCRIBE,RequestId,dict_to_wamp(Options),Topic];

to_wamp({subscribed,RequestId,SubscriptionId}) ->
  [?SUBSCRIBED,RequestId,SubscriptionId];

to_wamp({unsubscribe,RequestId,SubscriptionId}) ->
  [?UNSUBSCRIBE,RequestId,SubscriptionId];

to_wamp({unsubscribed,RequestId}) ->
  [?UNSUBSCRIBED,RequestId];

to_wamp({event,SubscriptionId,PublicationId,Details,undefined,undefined}) ->
  [?EVENT,SubscriptionId,PublicationId,dict_to_wamp(Details)];
to_wamp({event,SubscriptionId,PublicationId,Details,Arguments,undefined}) ->
  [?EVENT,SubscriptionId,PublicationId,dict_to_wamp(Details),Arguments];
to_wamp({event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw}) ->
  [?EVENT,SubscriptionId,PublicationId,dict_to_wamp(Details),Arguments,ArgumentsKw];

to_wamp({call,RequestId,Options,Procedure,undefined,undefined}) ->
  [?CALL,RequestId,dict_to_wamp(Options),Procedure];
to_wamp({call,RequestId,Options,Procedure,Arguments,undefined}) ->
  [?CALL,RequestId,dict_to_wamp(Options),Procedure,Arguments];
to_wamp({call,RequestId,Options,Procedure,Arguments,ArgumentsKw}) ->
  [?CALL,RequestId,dict_to_wamp(Options),Procedure,Arguments,ArgumentsKw];

to_wamp({cancel,RequestId,Options}) ->
  [?CANCEL,RequestId,dict_to_wamp(Options)];

to_wamp({result,RequestId,Details,undefined,undefined}) ->
  [?RESULT,RequestId,dict_to_wamp(Details)];
to_wamp({result,RequestId,Details,Arguments,undefined}) ->
  [?RESULT,RequestId,dict_to_wamp(Details),Arguments];
to_wamp({result,RequestId,Details,Arguments,ArgumentsKw}) ->
  [?RESULT,RequestId,dict_to_wamp(Details),Arguments,ArgumentsKw];

to_wamp({register,RequestId,Options,Procedure}) ->
  [?REGISTER,RequestId,dict_to_wamp(Options),Procedure];

to_wamp({registered,RequestId,RegistrationId}) ->
  [?REGISTERED,RequestId,RegistrationId];

to_wamp({unregister,RequestId,RegistrationId}) ->
  [?UNREGISTER,RequestId,RegistrationId];

to_wamp({unregistered,RequestId}) ->
  [?UNREGISTERED,RequestId];

to_wamp({invocation,RequestId,RegistrationId,Details,undefined,undefined}) ->
  [?INVOCATION,RequestId,RegistrationId,dict_to_wamp(Details)];
to_wamp({invocation,RequestId,RegistrationId,Details,Arguments,undefined}) ->
  [?INVOCATION,RequestId,RegistrationId,dict_to_wamp(Details),Arguments];
to_wamp({invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}) ->
  [?INVOCATION,RequestId,RegistrationId,dict_to_wamp(Details),Arguments,ArgumentsKw];

to_wamp({interrupt,RequestId,Options}) ->
  [?INTERRUPT,RequestId,dict_to_wamp(Options)];

to_wamp({yield,RequestId,Options,undefined,undefined}) ->
  [?YIELD,RequestId,dict_to_wamp(Options)];
to_wamp({yield,RequestId,Options,Arguments,undefined}) ->
  [?YIELD,RequestId,dict_to_wamp(Options),Arguments];
to_wamp({yield,RequestId,Options,Arguments,ArgumentsKw}) ->
  [?YIELD,RequestId,dict_to_wamp(Options),Arguments,ArgumentsKw];

to_wamp(noreply)  ->
  noreply;
to_wamp(shutdown)  ->
  shutdown.


dict_to_erl(Dict) ->
  convert_dict(to_erl,Dict,[]).

dict_to_wamp(Dict) ->
  convert_dict(to_wamp,Dict,[]).

-define(DICT_MAPPING,[
                      {agent,<<"agent">>,false},
                      {anonymous,<<"anonymous">>,false},
                      {authid,<<"authid">>,false},
                      {authmethod,<<"authmethod">>,false},
                      {authmethods,<<"authmethods">>,list},
                      {authprovider,<<"authprovider">>,false},
                      {authrole,<<"authrole">>,false},
                      {broker,<<"broker">>,dict},
                      {call_canceling,<<"call_canceling">>,false},
                      {call_timeout,<<"call_timeout">>,false},
                      {call_trustlevels,<<"call_trustlevels">>,false},
                      {callee,<<"callee">>,false},
                      {callee_blackwhite_listing,<<"callee_blackwhite_listing">>,false},
                      {caller,<<"caller">>,false},
                      {caller_exclusion,<<"caller_exclusion">>,false},
                      {caller_identification,<<"caller_identification">>,false},
                      {challenge,<<"challenge">>,false},
                      {dealer,<<"dealer">>,dict},
                      {disclose_me,<<"disclose_me">>,false},
                      {eligible,<<"eligible">>,false},
                      {event_history,<<"event_history">>,false},
                      {exclude,<<"exclude">>,false},
                      {exclude_me,<<"exclude_me">>,false},
                      {features,<<"features">>,dict},
                      {partitioned_pubsub,<<"partitioned_pubsub">>,false},
                      {partitioned_rpc,<<"partitioned_rpc">>,false},
                      {pattern_based_registration,<<"pattern_based_registration">>,false},
                      {pattern_based_subscription,<<"pattern_based_subscription">>,false},
                      {progress,<<"progress">>,false},
                      {progressive_call_results,<<"progressive_call_results">>,false},
                      {publication_trustlevels,<<"publication_trustlevels">>,false},
                      {publisher,<<"publisher">>,dict},
                      {publisher_exclusion,<<"publisher_exclusion">>,false},
                      {publisher_identification,<<"publisher_identification">>,false},
                      {receive_progress,<<"receive_progress">>,false},
                      {roles,<<"roles">>,dict},
                      {subscriber,<<"subscriber">>,dict},
                      {subscriber_blackwhite_listing,<<"subscriber_blackwhite_listing">>,false},
                      {subscriber_list,<<"subscriber_list">>,false},
                      {subscriber_metaevents,<<"subscriber_metaevents">>,false},
                      {wampcra,<<"wampcra">>,false}
                      ]).


convert_dict(to_wamp,[],[]) ->
  [{}];
convert_dict(to_wamp,[{}],[]) ->
  [{}];
convert_dict(to_erl,[{}],[]) ->
  [];
convert_dict(to_erl,[],[]) ->
  [];
convert_dict(_Direction,[],Converted) ->
  lists:reverse(Converted);
convert_dict(Direction,[{Key,Value}|T],Converted) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,

  {ErlKey,WampKey,Deep} =
    case lists:keyfind(Key,KeyPos,?DICT_MAPPING) of
      {Ek,Wk,D} -> {Ek,Wk,D};
      false -> {Key,Key,false}
    end,
  ConvValue =
    case Deep of
      dict -> convert_dict(Direction,Value,[]);
      list -> convert_list(Direction,Value,[]);
      _ -> Value
    end,
  ConvKey =
    case Direction of
      to_erl -> ErlKey;
      to_wamp -> WampKey
    end,
  convert_dict(Direction,T,[{ConvKey,ConvValue}|Converted]).


convert_list(_,[],[]) ->
  [];
convert_list(_,[],Converted) ->
  lists:reverse(Converted);
convert_list(Direction,[Key|T],Converted) ->
  KeyPos =
    case Direction of
      to_erl -> 2;
      to_wamp -> 1
    end,
  {ErlKey,WampKey} =
    case lists:keyfind(Key,KeyPos,?DICT_MAPPING) of
      {Ek,Wk,_} -> {Ek,Wk};
      false -> {Key,Key}
    end,
  ConvKey =
    case Direction of
      to_erl -> ErlKey;
      to_wamp -> WampKey
    end,
  convert_list(Direction,T,[ConvKey|Converted]).


-ifdef(TEST).

validation_test() ->
  true = is_valid_id(0),
  true = is_valid_id(9007199254740991),
  false = is_valid_id(9007199254740992),
  false = is_valid_id(-1),
  false = is_valid_id(0.1),

  true = is_valid_uri(<<"wamp.ws">>),
  true = is_valid_dict([]),

  true = is_valid_arguments([]),
  true = is_valid_argumentskw([]),
  ok.



hello_json_test() ->
  M = [?HELLO,<<"realm1">>,[{}]],
  S = serialize(M,json),
  D = deserialize(S,json),
  D = {[{hello,<<"realm1">>,[]}],<<"">>}.

hello_json_batched_test() ->
  M = [?HELLO,<<"realm1">>,[{}]],
  S = serialize(M,json_batched),
  D = deserialize(S,json_batched),
  D = {[{hello,<<"realm1">>,[]}],<<"">>}.

hello_msgpack_test() ->
  M = [?HELLO,<<"realm1">>,[{}]],
  S = serialize(M,msgpack),
  D = deserialize(S,msgpack),
  D = {[{hello,<<"realm1">>,[]}],<<"">>}.

hello_msgpack_batched_test() ->
  M = [?HELLO,<<"realm1">>,[{}]],
  S = serialize(M,msgpack_batched),
  D = deserialize(S,msgpack_batched),
  D = {[{hello,<<"realm1">>,[]}],<<"">>}.




hello_msgpack_deserialize_test() ->
  Data = <<147,1,166,114,101,97,108,109,49,130,165,97,103,101,110,
                      116,175,87,97,109,112,121,46,106,115,32,118,49,46,48,46,
                      51,165,114,111,108,101,115,132,169,112,117,98,108,105,
                      115,104,101,114,129,168,102,101,97,116,117,114,101,115,
                      131,189,115,117,98,115,99,114,105,98,101,114,95,98,108,
                      97,99,107,119,104,105,116,101,95,108,105,115,116,105,110,
                      103,195,179,112,117,98,108,105,115,104,101,114,95,101,
                      120,99,108,117,115,105,111,110,195,184,112,117,98,108,
                      105,115,104,101,114,95,105,100,101,110,116,105,102,105,
                      99,97,116,105,111,110,195,170,115,117,98,115,99,114,105,
                      98,101,114,128,166,99,97,108,108,101,114,129,168,102,101,
                      97,116,117,114,101,115,131,185,99,97,108,108,101,101,95,
                      98,108,97,99,107,119,104,105,116,101,95,108,105,115,116,
                      105,110,103,195,176,99,97,108,108,101,114,95,101,120,99,
                      108,117,115,105,111,110,195,181,99,97,108,108,101,114,95,
                      105,100,101,110,116,105,102,105,99,97,116,105,111,110,
                      195,166,99,97,108,108,101,101,129,168,102,101,97,116,117,
                      114,101,115,129,181,99,97,108,108,101,114,95,105,100,101,
                      110,116,105,102,105,99,97,116,105,111,110,195>>,
  {[{hello,<<"realm1">>,_}],_} = deserialize(Data,msgpack).



roundtrip_test() ->
  Messages = [
              {hello,<<"realm1">>,[]}
              ],
  Serializer = fun(Message,Res) ->

                 Encodings = [json,msgpack,raw_json,raw_msgpack,json_batched,msgpack_batched],

                 Check = fun(Enc,Bool) ->
                           EncMsg = serialize(Message,Enc),
                           DeEncMsg = deserialize(EncMsg,Enc) ,
                           case DeEncMsg of
                             {[Message],<<"">>} -> Bool;
                             _ -> false
                           end
                         end,
               Res and lists:foldl(Check,true,Encodings)
               end,

  true = lists:foldl(Serializer,true,Messages).



-endif.

