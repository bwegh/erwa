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

-module(erwa_sessions).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([create_table/0]).
-export([drop_table/0]).

-export([register_session/1]).
-export([unregister_session/0]).
-export([send_message_to/2]).


-record(erwa_session_record, {
					id = none,
					pid = unknown,
					realm = unknown 
				 }).


-spec create_table() -> ok.
create_table() -> 
	mnesia:create_table(erwa_session_record,[{disc_copies,[]}, {disc_only_copies, []},
																		 {ram_copies, [node()]}, {type, set},
																		 {attributes, record_info(fields,
																															erwa_session_record)},
																		 {index, [pid, realm]}]),
	mnesia:wait_for_tables([erwa_session_record],60000),
	ok.

-spec drop_table() -> ok.
drop_table() ->
	mnesia:delete_table(erwa_session_record),
	ok.


-spec send_message_to(Msg :: term(), SessionId :: non_neg_integer()) -> ok | {error, unknown}.
send_message_to(Msg,SessionId) ->
	GetPid = fun() -> mnesia:read({erwa_session_record, SessionId}) end,
  case  mnesia:transaction(GetPid) of
		{atomic,[Session]} ->
			Session#erwa_session_record.pid ! {erwa,Msg};
		{atomic,[]} ->
			{error,unknown}
  end.


-spec register_session(Realm :: binary()) -> {ok,non_neg_integer()}.
register_session(Realm) ->
	ID = add_session(self(), Realm),
	{ok, ID}.

-spec unregister_session() -> ok | not_found.
unregister_session() ->
	delete_session(self()).



add_session(Pid,Realm) ->
  ID = crypto:rand_uniform(0,9007199254740992),
	Data = #erwa_session_record{id=ID, pid=Pid, realm=Realm},
	AddSession = fun() -> case mnesia:read({erwa_session_record, ID}) of 
													[] -> mnesia:write(Data);
													_ -> mnesia:abort(already_exists)
												end 
							 end,
	case mnesia:transaction(AddSession) of
		{atomic, ok} ->
      ID;
		{aborted, already_exists}  ->
			add_session(Pid,Realm);
		Other ->
			ct:log("got an issue: ~p~n",[Other]),
			{error, Other}
	end.

delete_session(Pid) when is_pid(Pid) ->
	RemoveSession = fun() ->
											case mnesia:index_read(erwa_session_record, Pid, pid)	of 
												[] -> {atomic, not_found};
												[Session] -> mnesia:delete({erwa_session_record,
																										Session#erwa_session_record.id})
											end 
									end,
	case mnesia:transaction(RemoveSession) of
		{atomic, ok} -> ok;
		_ -> not_found
	end;
delete_session(ID) ->
	RemoveSession = fun() -> mnesia:delete({erwa_session_record, ID}) end,
	case mnesia:transaction(RemoveSession) of
		{atomic, ok} -> 
			ok;
    _ ->
      not_found
  end.

-ifdef(TEST).

simple_test() ->
	mnesia:start(),
	ok = create_table(),
	not_found = unregister_session(), 
  {ok,_} = register_session(<<"test_realm">>),
  ok = unregister_session(),
	ok = drop_table().
-endif.

