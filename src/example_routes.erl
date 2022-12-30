-module(example_routes).

%% ## Overview
%%
%% This is an example of how you declare routes to the router. Best to read the code side by side with `example_handlers`.
%%
%% If you want to add a new path you add a line here. The format is

%% * a path (with optional names)
%% * a security model associated with the path
%% * a dispatch tuple consisting of {Module, Function} where the function has an arity of 2 and must be exported
%% ^
%%
%% Obviously when you publish a URL that is expected to be nonced you gotta user the router export `make_nonce/3` to nonce it

-export([get_routes/0]).

%%
%% ### Security model macros
%%
%% Admins must be logged in to be admins
%%
%% If a segment requires a nonce it implies that it also requires a logged-in user
-define(PUBLIC,      {no_login, user}).
-define(USERLOGIN,   {login,    user}).
-define(USERNONCED,  {nonce,    user}).
-define(ADMINLOGIN,  {login,    admin}).
-define(ADMINNONCED, {nonce,    admin}).

%% ## Public API

get_routes() ->
    % The macros define the only logical sets of combinations so use them.
    %
    % You can turn a URL segment into a value that can be picked up with a prefix of a `:` so "/home/:user" will match "/home/gordon" and return a KV of `{"user", "gordon"}`
    [
        {"/",            ?PUBLIC,      {example_handlers, root}},
        {"/home/:user",  ?USERLOGIN,   {example_handlers, home}},
        {"/nonce",       ?USERNONCED,  {example_handlers, root}},
        {"/admin",       ?ADMINLOGIN,  {example_handlers, admin}},
        {"/admin/nonce", ?ADMINNONCED, {example_handlers, admin_action}}
    ].