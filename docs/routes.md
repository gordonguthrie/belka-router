```erlang
-module(routes).

```

-------------------------------------------------------------------

@doc routes

this module is used by the router
if you want to add a new path you add a line here
the format is
* a path (with optional names)
* a security model associated with the path
* a dispatch tuple consisting of {Module, Function}
  where the function has an arity of 2 and must be exported

obivously when you publish a URL that is expected to be nonced
you gotta user the router export `make_nonce/3` to nonce it

@end

-------------------------------------------------------------------

```erlang


-export([get_routes/0]).

```


Security model macros

admins must be logged in to be admins

```erlang
-define(PUBLIC,      {no_login, user}).
-define(USERLOGIN,   {login,    user}).
-define(USERNONCED,  {nonce,    user}).
-define(ADMINLOGIN,  {login,    admin}).
-define(ADMINNONCED, {nonce,    admin}).

get_routes() ->
```

the macros define the only logical sets of combinations
use them
you can turn a URL segment into a value that can be picked up
with a prefix of a `:`
so "/home/:user" will match "/home/gordon" and return a KV of `{"user", "gordon"}`

```erlang
    [
        {"/",            ?PUBLIC,      {example, root}},
        {"/home/:user",  ?USERLOGIN,   {example, home}},
        {"/nonce",       ?USERNONCED,  {example, root}},
        {"/admin",       ?ADMINLOGIN,  {example, admin}},
        {"/admin/nonce", ?ADMINNONCED, {example, admin_action}}
    ].
```