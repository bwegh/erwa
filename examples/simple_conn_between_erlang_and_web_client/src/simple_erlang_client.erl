-module(simple_erlang_client).
-behaviour(gen_server).

-define(RPC_SQUARE_URL,<<"ws.wamp.test.square">>).
-define(RPC_ECHO_URL,<<"ws.wamp.test.echo">>).
-define(EVENT_URL,<<"ws.wamp.test.info">>).
-define(KEEPALIVE_URL, <<"ws.wamp.test.keepalive">>).

%%-define(REALM,<<"ws.wamp.test">>).
-define(HOST,"localhost"). % has to be a string
-define(PORT,5555).
-define(ENCODING,msgpack). %% msgpack or json
-define(AWRE_OPTIONS, #{}).

-define(KEEPALIVE_SEND_INTERVAL, 1000).

-export([start_link/2]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state,{
  con = undefined,
  session = undefined,
  rpc_echo_id = undefined,
  event_sub_id = undefined,
  client = undefined,
  keepaliveTref
              }).

start_link(Client, Realm) ->
	gen_server:start_link({local, Client}, ?MODULE, [Client, Realm], []). 
	
init([Client, Realm]) ->
	error_logger:info_report(["Starting client", [{client, Client}]]),
	{ok,Con} = awre:start_client(),
	  
	error_logger:info_report(["Starting client - DONE", [{client, Client},
														 {con, Con}]]),
	
	error_logger:info_report(["Connecting to realm", [{client, Client},
													  {con, Con},
													  {realm, Realm},
													  {host, ?HOST},
													  {port, ?PORT},
													  {encoding, ?ENCODING}
													  ]]),
	{ok,SessionId,_RouterDetails} = awre:connect(Con,?HOST,?PORT,Realm,?ENCODING),
	  
	error_logger:info_report(["Connecting to realm - DONE", [{client, Client},
															 {con, Con}]]),
	
	error_logger:info_report(["Subscribe to event", [{client, Client},
													 {con, Con},
													 {subscribeTo, ?EVENT_URL}]]),
	
  	{ok,SubId} = awre:subscribe(Con,?AWRE_OPTIONS,?EVENT_URL),
	
	error_logger:info_report(["Subscribe to event - DONE", [{client, Client},
															{con, Con},
															{subscribeTo, ?EVENT_URL},
															{subscriptionId, SubId}]]),
  	
	error_logger:info_report(["Register to procedure", [{client, Client},
													{con, Con},
													{registerTo, ?RPC_ECHO_URL}]]),
	
  	{ok,EchoRPCId} = awre:register(Con,?AWRE_OPTIONS,?RPC_ECHO_URL),

	error_logger:info_report(["Register to procedure - DONE", [{client, Client},
														   {con, Con},
														   {registerTo, ?RPC_ECHO_URL},
														   {registrationId, EchoRPCId}]]),
	
	error_logger:info_report(["Client sucessfully initialized"]),
	
	error_logger:info_report(["If you send me an message on the subscribed event, I will call a procedure.", [{subscribeTo, ?EVENT_URL},
																											  {procedureToCall, ?RPC_SQUARE_URL}]]),
	{ok, TRef} = timer:send_interval(?KEEPALIVE_SEND_INTERVAL, {tick}),
	
	{ok,#state{con=Con,
			   session=SessionId,
			   rpc_echo_id=EchoRPCId,
			   event_sub_id=SubId, 
			   client = Client,
			   keepaliveTref = TRef
			  }}.

handle_call(_,_From,State) ->
  {noreply,State}.

handle_cast(_Msg,State) ->
  {noreply,State}.


handle_info({awre,{event,SubId,_PublicationId,_Details,Arguments,ArgumentsKw}},#state{event_sub_id=SubId,con=Con}=State) ->
	
	error_logger:info_report(["Event has been received in client.", [{client, State#state.client},
																	 {con, State#state.con},
																	 {subId, SubId},
																	 {publicationId, _PublicationId},
																	 {details, _Details},
																	 {arguments, Arguments},
																	 {argumentsKw, ArgumentsKw}
																	 ]]),
	
  	%% The number what must use for square operation should take from the Arguments.
	%% The Arguments is a list what contains string inside the binary, so the integer number should be grab from that.
	%% eq: [<<"2,4">>]
	[Bin] = Arguments,
	[NumberStr, ExpectedResultStr] = string:tokens(erlang:binary_to_list(Bin), ","),
	ExpectedResult = erlang:list_to_integer(ExpectedResultStr),
	Params = [erlang:list_to_integer(NumberStr)],
  	
	error_logger:info_report(["Client is calling the registered procedure.", [{client, State#state.client},
																			  {con, Con},
																			  {prcedureToCall, ?RPC_SQUARE_URL},
																			  {parameters, Params}
																			  ]]),
	
  	{ok,Details,[ResA],ResAKw} = awre:call(Con,?AWRE_OPTIONS,?RPC_SQUARE_URL,Params),
	
	error_logger:info_report(["Client sucessfully called the registered procedure.", 
							  [{client, State#state.client},
							   {con, Con},
							   {prcedureToCall, ?RPC_SQUARE_URL},
							   {parameters, Params},
							   {detailes, Details},
							   {resA, ResA},
							   {resAKw, ResAKw}
							  ]]),
	case ResA of
		ExpectedResult ->
			error_logger:info_report(["Result of procedure call is PASSED", 
									  [{expectedResult, ExpectedResult},
									   {computedResult, ResA}]]);
		_Err ->
			error_logger:error_report(["Result of procedure call is FAILED", 
									  [{expectedResult, ExpectedResult},
									   {computedResult, ResA}]])
	end,
	
	%%   io:format("unsubscribing from ~p ... ",[SubId]),
	%%   ok = awre:unsubscribe(Con,SubId),
	%%   io:format("unsubscribed.~n"),
	
  	{noreply,State};


handle_info({awre,{invocation,RequestId,RpcId,_Details,Arguments,ArgumentsKw}},#state{rpc_echo_id=RpcId,con=Con}=State) ->
  	%invocation of the echo rpc
  	error_logger:info_report(["The clinet had been called by another client", 
							  [{client, State#state.client},
							   {con, State#state.con},
							   {msgType, invocation},
							   {reqId, RequestId},
							   {rpcId, RpcId},
							   {detailes, _Details},
							   {arguments, Arguments},
							   {argumentsKw, ArgumentsKw},
							   {rpc_echo_id, State#state.rpc_echo_id},
							   {con, State#state.con},
							   {session, State#state.session}]]),
	
	error_logger:info_report(["Client will replies on that", 
							  [{client, State#state.client},
							   {con, State#state.con},
							   {reqId, RequestId},
							   {arguments, Arguments},
							   {argumentsKw, ArgumentsKw}
							   ]]),
  	ok = awre:yield(Con,RequestId,?AWRE_OPTIONS,Arguments,ArgumentsKw),
	
	error_logger:info_report(["Client has been sucessfully replied", 
							  [{client, State#state.client},
							   {con, State#state.con},
							   {reqId, RequestId},
							   {arguments, Arguments},
							   {argumentsKw, ArgumentsKw}
							   ]]),
	
	%%   ok = awre:unregister(Con,RpcId),
	%%   io:format("unregistered.~n"),
  	{noreply,State};

handle_info({tick}, State) ->
	Msg = string:concat("keepalive-", erlang:atom_to_list(State#state.client)),
	MsgBin = erlang:list_to_binary(Msg),
	case awre:publish(State#state.con, #{}, ?KEEPALIVE_URL, [MsgBin]) of
		ok ->
			error_logger:info_report(["Keepalive has been sent", [{msg, Msg}]]);
		Err ->
			error_logger:error_report(["Failed to send Keepalive", [{msg, Msg},
																	{reason, Err}]])
	end,
	{noreply, State};


handle_info(Msg,State) ->
	error_logger:info_report(["Unexpected event has been received by client", 
							  [{client, State#state.client},
							   {con, State#state.con},
							   {msg, Msg},
							   {state, State}
							   ]]),
  	{noreply,State}.

terminate(Reason, State) ->
	error_logger:info_report(["Terminate Client", 
							  [{reason, Reason},
							   {state, State}]]),
	awre:unregister(State#state.con, State#state.rpc_echo_id),
	awre:unsubscribe(State#state.con, State#state.event_sub_id),
	
  	ok.

code_change(_OldVsn,State,_Extra) ->
  	{ok,State}.

