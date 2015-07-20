%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2015 12:02
%%%-------------------------------------------------------------------
-author("tihon").

-record(session,
{
	id = none,
	is_auth = false,
	realm_name = none,
	mwl = [],
	client_roles = unknown,
	routing_pid = none,
	broker = none,
	dealer = none,
	source = unknown,
	peer = unknown,
	ssl = false,
	trans = unknown,
	goodbye_sent = false,
	calls = [],
	session_data = #{},
	authid = anonymous,
	role = guest,
	will_pass = false,
	invocation_id = 1,
	invocations = []
}).

-record(tcp_state, {
	socket,
	transport,
	ok,
	closed,
	error,
	erlbin_number = undefined,
	enc = undefined,
	length = infitity,
	buffer = <<"">>,
	session = undefined
}).

-record(ws_state, {
  enc = undefined,
  ws_enc = undefined,
  length = infitity,
  buffer = <<"">>,
  session = undefined
}).


-define(BROKER_FEATURES,
  #{
    features =>
    #{
      event_history => false,
      partitioned_pubsub => false,
      pattern_based_subscription => false,
      publication_trustlevels => false,
      publisher_exclusion => true,
      publisher_identification => true,
      subscriber_blackwhite_listing => true,
      subscriber_list => false,
      subscriber_metaevents => false
    }
  }
).

-define(DIALER_FEATURES,
  #{
    features =>
    #{
      call_canceling => true,
      call_timeout => true,
      call_trustlevels => false,
      callee_blackwhite_listing => false,
      caller_exclusion => false,
      caller_identification => false,
      partitioned_rpc => false,
      pattern_based_registration => false,
      progressive_call_results => true
    }
  }
).

-record(topic,
{
  uri = unknown,
  id = none,
  match = exact,
  created = unknown,
  subscribers = []
}).

-record(id_topic,
{
  id = none,
  topic = unknown
}).

-record(id_broker_info,
{
  id = unknown,
  topics = []
}).

-record(data,
{
  ets = none,
  pid = unknown,
  features
}).

-record(id_procedure,
{
  id = none,
  uri = none
}).

-record(id_dialer_info,
{
  id = unknown,
  procs = []
}).

-record(procedure,
{
  uri = none,
  id = none,
  created = unknown,
  match = exact,
  invoke = single,
  options = [],
  ids = []
}).