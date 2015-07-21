%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2015 18:04
%%%-------------------------------------------------------------------
-module(erwa_support).
-author("tihon").

%% API
-export([get_peer/1, nonce/0, gen_id/0]).

get_peer(Socket) ->
  inet:peername(Socket).

nonce() ->
  base64:encode(crypto:strong_rand_bytes(15)).

gen_id() ->
  crypto:rand_uniform(0, 9007199254740992).