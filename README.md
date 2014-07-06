erwa
====

Erwa is a [WAMP v2](http://wamp.ws/spec/) (Web Application Messaging Protocol) implementation in Erlang,
both peer and router.




Table of Contents
=================

* [Description](#description)
* [Router](#router)
* [Peer](#peer)
* [Examples](#examples)
* [Licencse](#license)


Description
===========
Erwa is the implementation of the WAMP protocol in Erlang.
It supports the router and the peer role, so the client and
the server side.

The project is still in early development, so I am happy to get any
kind of feedback.

[Back to TOC](#table-of-contents)


Router
======
The router implementation in Erwa uses the great [ranch](https://github.com/extend/ranch)
and [cowboy](https://github.com/extend/cowboy) from [Loïc Hoguin (essen)](https://github.com/essen)
to handle the incomming connections and the webserver part for the websockets.
Erwa has two modules to work either as a protocol for ranch on incomming TCP connections, or
as websocket handler with cowboy on incomming websocket connections.

The features of the router are:
* the basic profile
* both router roles
  * Dealer
  * Broker

All you need to do to get a simple WAMP router up and running is to add a dispatch rule to
ranch and/or cowboy:

A WAMP router on websockets:
* using erwa_ws_handler as the websocket handler, by dispatching a certain path to conditions
* starting cowboy on a certain port (here 8080) and add the dispatch rule
```Erlang
%% a rule to dispatch incomming connections to any host with the path /wamp to the erwa_ws_handler
Dispatch = cowboy_router:compile([ {'_', [ {"/wamp", erwa_ws_handler, []}, ]} ]),
%% fire up cowboy with the dispatch rule for the wamp connection
{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[{env, [{dispatch, Dispatch}]}]),
```
In the examples directory you can find the simple_router which includes just the above
and starts a WAMP router, including a simple javascript client,
using [wampy.js](https://github.com/KSDaemon/wampy.js).

The other possibility is to start Erwa as a TCP router:
Erwa implements a protocol for ranch in the erwa_tcp_handler modules.
So starting and tcp router is done by starting ranch with
erwa_tcp_handler as the protocol:
```Erlang
%% start ranch with the wamp protocol by using erwa_tcp_handler on port 555
{ok,_} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_tcp_handler, []),
```
This is also included in the simple_router example in the examples directory.

By default Erwa does not automatically create realms. This is activated by the boolean
*autocreate_realm* flag.
If you only want to support certain realm you need to start them by using erwa:start_realm/1,
which takes the name of the realm to start as argument.

[Back to TOC](#table-of-contents)


Peer
====
Erwa implements the basic profile for all four roles of a peer, caller and callee as well as
publisher and subscriber.
The connection to the router can either be remote to another host/port or local to a router
running within the same VM.

The connections are implemented as gen_server and can be shared between different processes.
The Idea is to have just one connection to a router and share it with all needing processes.
*A lookup of connections to certain routers and realms is not yet implemented, yet under consideration*

To connect to a realm you need to follow a few simple steps:
```Erlang
%% first start a connection
{ok,Con} = erwa:start_client(),
%% then connect to either a local or remote router
%% local would be {ok,SessionId,RouterDetails} = erwa:connect(Con,Realm,Encoding),
%% the following is a remote router
{ok,SessionId,RouterDetails} = erwa:connect(Con,Host,Port,Realm,Encoding),

%% now the connection 'Con' is connected to the router and handles everything for you
%% sending an event to a certain topic is just as easy:
%% the two parameters Arguments and ArgumentsKW are optional.
ok = erwa:publish(Con,Options,EventTopicUrl,Arguments,ArgumentsKW),

%% invoking a remote procedure is easy, too:
%% as with publishing are the Arguments and the ArgumentsKW optional.
%% ResA and ResAKw may be undefined in case they did not include any data.
{ok,Details,ResA,ResAKw} = erwa:call(Con,Options,ProcedureUri,Arguments,ArgumentsKW),

%% for subscription to a topic or registering a function for remote calls
%% there exist two different ways:
%% one is to use a tuple of module, function and one argument that will be called by erwa.
%% this is the easiest way, yet has the drawback that you can only forward one argument
%% to your function and not return any state change or similar.
%% the following line will invoke Module:Function(Details,Arguments,ArgumentsKW,OneArgument)
%% on the occurance of an event.
{ok,SubscriptionId} = erwa:subscribe(Con,Options,EventUrl,{Module,Function,OneArgument}),

%% the same works for registering a remote procedure.
%% the function that will be called here is also:
%% Module:Function(Details,Arguments,ArgumentsKW,OneArgument)
{ok,RegistrationId} = erwa:register(Con,Options,ProcedureUri,{Module,Function,OneArgument}),


%% the other possibility is to subscribe or register without giving an mfa.
%% In this case the process will receive the
%% {erwa,{event,SubscriptionId,PublicationId,Details,Arguments,ArgumentsKw}}
%% message for an event and the
%% {erwa,{invocation,RequestId,RegistrationId,Details,Arguments,ArgumentsKw}}
%% message for an invocation.
%% you MUST reply to an invocation by using erwa:yield/3,erwa:yield/4 or erwa:yield/5.
%%
{ok,SubscriptionId} = erwa:subscribe(Con,Options,EventUri),
{ok,RegistrationId} = erwa:register(Con,Options,ProcedureUri),
```

The crossbar_client example includes registration and subscriptions using mfa.
Also have a look at the simple_client example for a client implementation using gen_server
and subscribe and register without mfa.

[Back to TOC](#table-of-contents)


Examples
========

In the exampes directory you can find three different examples:
 * crossbar_client: This is the template used in crossbar to demonstrate the usage of an erlang client with crossbar.io.
 * simple_client: A simple client that shows how a client can be implemented using a gen_server.
 * simple_router: A very tiny example, showing how easy it is to fire up a WAMP router using Erwa.

[Back to TOC](#table-of-contents)


Licencse
========
The MIT License (MIT)

Copyright (c) 2014 Bas Wegh

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
[Back to TOC](#table-of-contents)
