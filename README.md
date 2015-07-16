Erwa
====
[![passing or failing?](https://travis-ci.org/bwegh/erwa.svg?branch=master)](https://travis-ci.org/bwegh/erwa/)


Erwa is an implementation of [WAMP (Web Application Messaging Protocol)](http://wamp.ws/spec/) router in Erlang.
The client/peer side has been moved to a seperate project called [Awre](https://github.com/bwegh/awre). Both use the library [Wamper](https://github.com/bwegh/wamper).



Table of Contents
=================

* [Description](#description)
* [Features](#features)
* [Router](#router)
* [Peer](#peer)
* [Examples](#examples)
* [License](#license)


Description
===========
Erwa is the implementation of the WAMP protocol in Erlang.
It supports the router, so the server side.

Pull Requests, Bug Reports, Comments and any other kind of feedback is welcome.


[Back to TOC](#table-of-contents)

Features
========

Erwa has the following features:
  * the complete [basic profile](https://github.com/tavendo/WAMP/blob/master/spec/basic.md) revision RC4
  * [Advanced Profile](https://github.com/tavendo/WAMP/blob/master/spec/advanced.md) (only listing stable/implemented)
     * RawSocket Transport
     * Batched Websocket Transport
     * Messages
     * Feature Announcement
     * Agent Identification
     * Subscriber Black-and Whitelisting
     * Publisher Exclusion
     * Publisher Identification
     * Progressive Call Results
     * Cancelling Calls
     * Call Timeouts
     * Caller Identification
  * NOT yet supported
     * LongPoll Transport
     * Challenge Response Authentication (Database needs to be implemented by user)

[Back to TOC](#table-of-contents)


Router
======
The router implementation in Erwa uses the great [ranch](https://github.com/ninenines/ranch)
and [cowboy](https://github.com/ninenines/cowboy) from [Loïc Hoguin (essen)](https://github.com/essen)
to handle the incomming connections and the webserver part for the websockets.
Erwa has two modules to work either as a protocol for ranch on incomming TCP connections, or
as websocket handler with cowboy on incomming websocket connections.

All you need to do to get a simple WAMP router up and running is to add a dispatch rule to
ranch and/or cowboy.  
A WAMP router on websockets:

    ok = erwa:start_websocket("/wamp", 8080, 100).
Which starts `cowboy` on 8080 port with 100 acceptors and use `erwa_in_handler` for `"/wamp"` path.  
If you wan't (for some reason) to use your own path on the same port - you can use `erwa:start_websocket/4` 
and pass a list of rules as a fourth parameter:  

    ok = erwa:start_websocket("/wamp", 8080, 100, [{"/example", example_handler, []}]).
In the examples directory you can find the simple_router which includes just the above
and starts a WAMP router, including a simple javascript client,
using [wampy.js](https://github.com/KSDaemon/wampy.js).

The other possibility is to start Erwa as a TCP router.  
A WAMP router on tcp sockets:  

    ok = erwa:start_socket(5555, 5).
Which starts `ranch` on 5555 port with 5 acceptors with `erwa_in_handler`.
This is also included in the simple_router example in the examples directory.

By default Erwa does not automatically create realms. This is activated by the boolean
*autocreate_realm* flag.
If you only want to support certain realm you need to start them by using erwa:start_realm/1,
which takes the name of the realm to start as argument.

[Back to TOC](#table-of-contents)




Examples
========

In the exampes directory you can find a simple router example:
 * simple_router: A very tiny example, showing how easy it is to fire up a WAMP router using Erwa.

[Back to TOC](#table-of-contents)


License
========
The MIT License (MIT)

Copyright (c) 2014-2015 Bas Wegh

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

#wampws
