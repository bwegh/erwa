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

-module(erwa_publications).

%% API
-export([create_table/0]).
-export([drop_table/0]).
-export([get_pub_id/0]).


-record(erwa_pub_rec,{
          id = unknown,
          other = unused
               }).

-spec create_table() -> ok.
create_table() ->
	case lists:member(erwa_pub_rec, mnesia:system_info(local_tables)) of
		true ->
			mnesia:delete_table(erwa_pub_rec);
		_-> do_nthing
	end,
	
    {atomic, ok} = mnesia:create_table(erwa_pub_rec, [{disc_copies,[]}, {ram_copies, [node()]},
                                       {type, set}, {attributes,
                                                     record_info(fields,
                                                                 erwa_pub_rec)}]),
    ok.

-spec drop_table() -> ok.
drop_table() -> 
    mnesia:delete_table(erwa_pub_rec),
    ok.

-spec get_pub_id() -> {ok,non_neg_integer()}.
get_pub_id() ->
    new_pub_id().


new_pub_id() ->
  ID = crypto:rand_uniform(0,9007199254740992),
  Data = #erwa_pub_rec{id=ID},
  Trans = fun() -> 
                  case mnesia:read({erwa_pub_rec, ID}) of 
                      [] -> 
                          mnesia:write(Data);
                      _ -> 
                          mnesia:abort(already_exists)
                  end
          end,
  case mnesia:transaction(Trans) of 
      {atomic, ok} -> 
          {ok, ID};
      {aborted, already_exists} ->
          new_pub_id()
  end.


