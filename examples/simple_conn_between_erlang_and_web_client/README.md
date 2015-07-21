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
Usage:
	- open a web browser and load http://localhost:8080 or https://localhost:8443
	- press on connect button
	- select square in the function's drop-down menu
	- click on register button
	- in the Erlang VM start a new client by the following command
		simple_conn_between_erlang_and_web_client_sup:start_client(c1, "realm1").
	- go back to web browser and type some words in data filed in the top of page
	- click on publish button
	- in the Erlang VM shell the next should be appears
		(simple_conn_between_erlang_and_web_client@127.0.0.1)8> received event [<<THE_TEXT_WHAT_YOU_HAVE_ENTERED>>] undefined on [899260646093393]
		calling <<"ws.wamp.test.square">> [3] ... result is: "\t" undefined [#{}]
	- Type some words in the field beside in call button
	- press on call button
	- the next should be seen in the Erlang VM's shell
		been called [61813386230299] with params [<<THE_TEXT_WHAT_YOU_HAVE_ENTERED>>] undefined ... will just send them back ...
	- the entered text should be appeared in the web page as well
	
Prepare deps/ranch for able to use SSL
	- open deps/ranch/src/ranch.app.src
	- add ssl at the end of the  list of applications, like this:
		{applications,[kernel,stdlib,ssl]}
