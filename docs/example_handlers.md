```erlang
-module(example_handlers).

```

## Overview

These are example handlers - each one is defined in the routing table returned by `example_routes:get_routes/0`

Best to read this side by side with `example_routes`.

```erlang

-export([
			root/2,
			home/2,
			admin/2,
			admin_action/2
		]).

```

## Public API

```erlang

root(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in example:root Path is ~p Vals is~p~n", [Path, Vals]),
	["20 text/gemin\r\nyou are in root\r\n"].

home(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in example:home Path is ~p Vals is ~p~n", [Path, Vals]),
	["20 text/gemini\r\nhome\r\n"].

admin(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in example:admin Path is ~p Vals is ~p~n", [Path, Vals]),
	[
		"20 text/gemini\r\n",
		"Welcome Administrator\r\n",
		"=> admin/nonce2F24DC1FE7AEEB1E8F8BC906232A7DB9 try and do something\r\n"
	].

admin_action(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in example:admin/nonce Path is ~p Vals is ~p~n", [Path, Vals]),
	[
		"20 text/gemini\r\n",
		"Shake some action\r\n"
	].

```
