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
-record(state,{
               router = undefined,
               realm = undefined
               }).


-export([create/0]).
-export([handle/2]).
-export([close/2]).
-export([to_wamp/1]).
-export([to_erl/1]).

% @TODO: validate all message parts and if not ok just crash it ...

create() ->
  #state{}.


handle(Wamp,State) ->
  Msg = to_erl(Wamp),
  {ok,Reply,NewState} = handle_message(Msg,State),
  WampReply = to_wamp(Reply),
  {ok,WampReply,NewState}.



close(Reason,#state{router=R}) ->
  erwa_router:remove_session(R,Reason).



% **************** Session Begin/End **************************
handle_message({hello,Realm,Details},#state{router=undefined}=State) ->
  {Reply,Router} =
    case erwa_realms:get_router(Realm) of
      {ok,R} ->
        {erwa_router:hello(R,Details),R};
      {error,_} ->
        {shutdown,undefined}
    end,
   {ok,Reply,State#state{realm=Realm, router=Router}};
handle_message({goodbye,_,goodbye_and_out},State) ->
  {ok,shutdown,State};
handle_message({goodbye,_,_},State) ->
  %TODO: remove the connection or maybe on closing when listening ... or so ...
  {ok,{goodbye,[{}]},goodbye_and_out,State};



% **************** Subscription and Events **************************
handle_message({subscribe,RequestId,Options,Topic},#state{router=Router}=State) ->
  Reply = erwa_router:subscribe(Router,RequestId,Options,Topic),
  {ok,Reply,State};
handle_message({unsubscribe,RequestId,SubscriptionId},#state{router=Router}=State) ->
  Reply = erwa_router:unsubscribe(Router,RequestId,SubscriptionId),
  {ok,Reply,State};

handle_message({publish,RequestId,Options,Topic,Arguments,ArgumentsKw},#state{router=Router}=State) ->
  Reply =  erwa_router:publish(Router,RequestId,Options,Topic,Arguments,ArgumentsKw),
  {ok,Reply,State};





% **************** Remote Procedure calls **************************

handle_message({register,RequestId,Options,ProcedureUrl},#state{router=Router}=State) ->
  Reply = erwa_router:register(Router,RequestId,Options,ProcedureUrl),
  {ok,Reply,State};
handle_message({unregister,RequestId,RegistrationId},#state{router=Router}=State) ->
  Reply = erwa_router:unregister(Router,RequestId,RegistrationId),
  {ok,Reply,State};

handle_message({call,RequestId,Options,Procedure,Arguments,ArgumentsKw},#state{router=Router}=State) ->
  Reply = erwa_router:call(Router,RequestId,Options,Procedure,Arguments,ArgumentsKw),
  {ok,Reply,State};

handle_message({yield,InvocationId,Options,Arguments,ArgumentsKw},#state{router=Router}=State) ->
  Reply = erwa_router:yield(Router,InvocationId,Options,Arguments,ArgumentsKw),
  {ok,Reply,State};



handle_message(_,State) ->
  {ok,{goodbye,[{message, <<"unknown message">>}],invalid_argument},State}.








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
  {hello,Realm,Details};

to_erl([?WELCOME,SessionId,Details]) ->
  {welcome,SessionId,Details};

to_erl([?ABORT,Details,Reason]) ->
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
  {goodbye,Details,Reason};

to_erl([?ERROR,RequestType,RequestId,Details,Error]) ->
  to_erl([?ERROR,RequestType,RequestId,Details,Error,undefined,undefined]);
to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments]) ->
  to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments,undefined]);
to_erl([?ERROR,RequestType,RequestId,Details,Error,Arguments,ArgumentsKw]) ->
  {error,RequestType,RequestId,Details,Error,Arguments,ArgumentsKw};

to_erl([?PUBLISH,RequestId,Options,Topic]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,undefined,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments]) ->
  to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,undefined]);
to_erl([?PUBLISH,RequestId,Options,Topic,Arguments,ArgumentsKw]) ->
  {publish,RequestId,Options,Topic,Arguments,ArgumentsKw};

to_erl([?PUBLISHED,RequestId,PublicationId]) ->
  {published,RequestId,PublicationId};

to_erl([?SUBSCRIBE,RequestId,Options,Topic]) ->
  {subscribe,RequestId,Options,Topic};

to_erl([?UNSUBSCRIBE,RequestId,SubscriptionId]) ->
  {unsubscribe,RequestId,SubscriptionId};

to_erl([?UNSUBSCRIBED,RequestId]) ->
  {unsubscribed,RequestId};

to_erl([?EVENT,SubscriptionId,PublicationId,Details]) ->
  to_erl([?EVENT,SubscriptionId,PublicationId,Details,undefined,undefined]);
to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments]) ->
  to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments,undefined]);
to_erl([?EVENT,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw]) ->
  {event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw};
to_erl([?CALL,RequestId,Options,Procedure]) ->
  to_erl([?CALL,RequestId,Options,Procedure,undefined,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments]) ->
  to_erl([?CALL,RequestId,Options,Procedure,Arguments,undefined]);
to_erl([?CALL,RequestId,Options,Procedure,Arguments,ArgumentsKw]) ->
  {call,RequestId,Options,Procedure,Arguments,ArgumentsKw};

to_erl([?RESULT,RequestId,Details]) ->
  to_erl([?RESULT,RequestId,Details,undefined,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments]) ->
  to_erl([?RESULT,RequestId,Details,Arguments,undefined]);
to_erl([?RESULT,RequestId,Details,Arguments,ArgumentsKw]) ->
  {result,RequestId,Details,Arguments,ArgumentsKw};

to_erl([?REGISTER,RequestId,Options,Procedure]) ->
  {register,RequestId,Options,Procedure};

to_erl([?REGISTERED,RequestId,RegistrationId]) ->
  {registered,RequestId,RegistrationId};

to_erl([?UNREGISTER,RequestId,RegistrationId]) ->
  {unregister,RequestId,RegistrationId};

to_erl([?UNREGISTERED,RequestId]) ->
  {unregistered,RequestId};

to_erl([?INVOCATION,RequestId, RegistrationId, Details]) ->
  to_erl([?INVOCATION,RequestId, RegistrationId, Details, undefined, undefined]);
to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments]) ->
  to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments, undefined]);
to_erl([?INVOCATION,RequestId, RegistrationId, Details, Arguments, ArgumentsKw]) ->
  {invocation,RequestId, RegistrationId, Details, Arguments, ArgumentsKw};

to_erl([?YIELD, RequestId, Options]) ->
  to_erl([?YIELD, RequestId, Options, undefined, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments]) ->
  to_erl([?YIELD, RequestId, Options, Arguments, undefined]);
to_erl([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
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



