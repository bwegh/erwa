Erwa simple router example
==========================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/simple_conn_between_erlang_and_web_client/bin/simple_router console
```

Prepare deps/ranch for able to use SSL
	- open deps/ranch/src/ranch.app.src
	- add ssl at the end of the  list of applications, like this:
		{applications,[kernel,stdlib,ssl]}

Then point your browser twice to port (HTTP and/or HTTPS), it depends of your choose
 - http: 8080
 - https:8443

Play around with the client.

Also have a look at [the Wamp specification](http://wamp.ws/spec) for further
informations on the possibilities.
