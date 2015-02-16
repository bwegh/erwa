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

-module(erwa_routing).

-export([create/0]).
-export([handle_messages/2]).
-export([handle_message/2]).


-record(state, {
                is_auth = false,
                router = undefined
                }).


create() ->
  #state{}.

handle_messages([], State) ->
  {ok, State};
handle_messages([M|Messages], State) ->
  {ok, NewState} = handle_message(M,State),
  handle_messages(Messages,NewState).



handle_message(Message,#state{router=Router} = State) ->
  {ok, NewRouter} = erwa_router:forward_messages([Message],Router),
  {ok, State#state{router=NewRouter}}.
