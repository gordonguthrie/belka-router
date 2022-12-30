%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

%% # Overview
%% The belka router is used in the Belka Gemini server to match incoming Gemini URLs to resources. It picks up its routes from a module passed in as an environment variable

-module(belka_router).

% It is an Erlang/OTP Gen Server
% [Erlang/OTP Gen Servers](http://erlang.org/doc/design_principles/gen_server_concepts.html)
-behaviour(gen_server).

% ## API for OTP declarations
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% ## OTP API Callbacks declarations
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

% ## Normal Usage API v
-export([
         dispatch/1,
         get_nonce/2
         ]).

% ## Developers API declarations (not for production use)
% When you are writing routes and building an app it is useful to be able to edit things on the fly. On startup the routing server
-export([
         recompile_routes/0,
         toggle_debug/0
         ]).

% ## Callbacks API declarations
% Exported for callbacks from within this module
-export([
         '51'/2,
         '60'/2,
         '60 (nonce)'/2,
         '60 (hacker)'/2,
         make_nonce/3
         ]).

% The state that the gen server stores.

-record(state, {routes = [], salt = "", admins = [], debug = false}).

% ## Bog standard OTP API - shared by all Erlang/OTP servers

start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->
    start_link(#{}).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

% ## Normal Usage API

% We do two things in normal usage in our app:
%
% * when the Belka server gets a `gemini://` request in, it asks the router what functions we should call to handle the request with `dispatch`
% * when we need to create a URL with a valid nonce, or check that the nonce supplied is valid we have to request that the router generates a nonce.
% ^

dispatch(Route) ->
    {{M, F}, Vals} = gen_server:call(?MODULE, {route, Route}),
    apply(M, F, [Route, Vals]).

get_nonce(URL, Id) ->
    gen_server:call(?MODULE, {get_nonce, {URL, Id}}).

% ## Developer API

% To make it easier for the developer of a new app we have a couple of helper functions that smooth things out when you are gradually adding routes and building out an application
%
% * `recompile_routes/0` tells the server to get the routes from the predefined routes module and recompile and save them
% * `toggle_debug/0` toggles a debug switch. If debug is `on` then print statements will dump information into the shell that will help you work out why the router is making the routing decisions it is.
% ^

recompile_routes() ->
    gen_server:call(?MODULE, recompile_routes).

toggle_debug() ->
    gen_server:call(?MODULE, toggle_debug).

% ## Callbacks

% The router doens't handle the requests itself - the calling process does, so when this router passes back an error handler, that handler needs to be an exported function. (These functions can also be called from within other handlers, as the router doesn't capture the complete granularity of the error space).

'51'(Route, Vals) ->
    io:format("in 51 Route is ~p~nVals is ~p~n", [Route, Vals]),
    [<<"51 Welcome to Area 51 👽\r\n"/utf8>>].

'60'(Route, Vals)->
    io:format("in 60 Route is ~p~nVals is ~p~n", [Route, Vals]),
    [<<"60 Criminal Code Section 60 Violation 👮\r\n"/utf8>>].

'60 (nonce)'(Route, Vals)->
    io:format("in 60 nonce Route is ~p~nVals is ~p~n", [Route, Vals]),
    [<<"60 Criminal Code Section 60 Violation - you're awa the jail ya nonce 🚓\r\n"/utf8>>].

'60 (hacker)'(Route, Vals)->
    io:format("in 60 hacker Route is ~p~nVals is ~p~n", [Route, Vals]),
    [<<"60 Criminal Code Section 60 Violation - back off hacker ☠️\r\n"/utf8>>].

%% ## The normal internal functions of the Gen Server

%% `init/1` is called when the gen server starts, it gets a list of configuration items from `sys.config` and then uses them to call the function that defines the routes and compile and store all of them.

init(_Args) ->
    true = register(?MODULE, self()),
    {ok, {M, F}} = application:get_env(belka_router, routes),
    {ok, Salt}   = application:get_env(belka_router, salt),
    {ok, Admins} = application:get_env(belka_router, admins),
    Routes = apply(M, F, []),
    CompiledRoutes = compile_routes(Routes),
    {ok, #state{routes = CompiledRoutes,
                salt   = Salt,
                admins = Admins}}.

% These two function heads are where the gen server handles the normal API call messages

handle_call({route, Route}, _From, State) ->
    Routes = State#state.routes,
    Salt = State#state.salt,
    Admins = State#state.admins,
    debug(State, "handling route for ~p~n", [Route]),
    Reply = get_dispatch(Route, Routes, Salt, Admins, State#state.debug),
    debug(State, "Reply is ~p~n", [Reply]),
    {reply, Reply, State};

handle_call({get_nonce, {URL, Id}}, _From, State) ->
    Salt = State#state.salt,
    Nonce = make_nonce(URL, Id, Salt),
    debug(State, "URL is ~p Nonce is ~p~n", [URL, Nonce]),
    {reply, Nonce, State};

% These two function heads are where the gen server handles the developer messages

handle_call(toggle_debug, _From, State) ->
    NewState = case State#state.debug of
        true  -> State#state{debug = false};
        false -> State#state{debug = true}
    end,
    {reply, ok, NewState};

handle_call(recompile_routes, _From, State) ->
    {ok, {M, F}} = application:get_env(belka_router, routes),
    Routes = apply(M, F, []),
    CompiledRoutes = compile_routes(Routes),
    debug(State, "Recompiled Routes are ~p~n", [CompiledRoutes]),
    NewState = State#state{routes = CompiledRoutes},
    {reply, ok, NewState}.

% We do nothing on either cast of info messages.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

% Standard do nothing on terminate or code reload

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ## Internal functions

debug(false,                 _, _)       -> ok;
debug(#state{debug = false}, _, _)       -> ok;
debug(_,                     Text, Args) -> io:format(Text, Args).

get_dispatch(Route, Routes, Salt, Admins, Debug) ->
    match_route(Routes, Route, Salt, Admins, Debug).

% This function is the heavy lifting of the router. An incoming message which has had its URL parsed down to components is checked in turn against each route defined in the list of routes. There are three exit routes:
%
% * a path can match perfectly - exit with a handler
% * a path can match but some other attribute is wrong in which case the search ends with an error, eg:
%       * good path, but not logged in
%       * good path but not administrator
%       * good path but bad nonce
% * no paths match in which case an ***Area `51`*** error is thrown
% ^

match_route([], _, _Salt, _Admins, Debug) ->
    debug(Debug, "no routes match 51~n", []),
    {{belka_router, '51'}, []};
match_route([H | T], Route, Salt, Admins, Debug) ->
    #{path := GotPath, id := Id} = Route,
    #{path := ExpPath, needs_login := Login, is_admin := IsAdmin, dispatch := MF} = H,
    debug(Debug, "testing ~p against ~p~n", [GotPath, ExpPath]),
    debug(Debug, "needs login? ~p is_admin? ~p dispatch ~p~n", [Login, IsAdmin, MF]),
    case match_path(GotPath, ExpPath, []) of
        no_match  ->
            debug(Debug, "no route, try next~n", []),
            match_route(T, Route, Salt, Admins, Debug);
        {check_nonce, Vals} ->
            debug(Debug, "check the nonce~n", []),
            handle_nonce_check(GotPath, Id, MF, Vals, Salt, IsAdmin, Admins, Debug);
        {match, Vals} ->
            debug(Debug, "got a match with ~p~n", [Vals]),
            case {Id, Login, IsAdmin} of
                {no_identity, login, _} ->
                    debug(Debug, "no identity - fail 60~n", []),
                    {{belka_router, '60'}, []};
                {_, login, admin} ->
                    debug(Debug, "check admin~n", []),
                    case check_admin(Admins, Id) of
                        {invalid, Error} ->
                            debug(Debug, "not admin - fail 60~n", []),
                            Error;
                        _ ->
                            debug(Debug, "all good for admin dispatch ~p ~p~n", [MF, Vals]),
                            {MF, Vals}
                    end;
                {_, _, user} ->
                    debug(Debug, "all good for users dispatch ~p ~p~n", [MF, Vals]),
                    {MF, Vals}
            end
    end.

% The `match_path` function checks if:
%
% * the path matches outright
% * the path matches subject to nonce checks
% * the path doesn't match outright
% ^
% It descends the segments in the path and match definitions from the top.

match_path([],        [],                Acc) -> {match, Acc};
match_path([H | T1],  [H | T2],          Acc) -> match_path(T1, T2, Acc);
match_path([H | T1],  [{Name, []} | T2], Acc) -> match_path(T1, T2, [{Name, H} | Acc]);
match_path([_H | []], [nonce | []],      Acc) -> {check_nonce, Acc};
match_path(_,         _,                _Acc) -> no_match.

% There are two sorts of nonce checks:
%
% * normal user nonces
% * admin user nonces
% ^
% So this function has to perform the `is_admin` check too

handle_nonce_check(GotPath, Id, MF, Vals, Salt, IsAdmin, Admins, Debug) ->
    case Id of
        no_identity ->
            debug(Debug, "nonce out - not logged in~n", []),
            {{belka_router, '60'}, []};
        _ ->
            [Nonce | Rest] = lists:reverse(GotPath),
            OrigPath = string:join(lists:reverse(Rest), "/"),
            ExpNonce = make_nonce(OrigPath, Id, Salt),
            debug(Debug, "Nonce is ~p ExpNonce is ~p~n", [Nonce, ExpNonce]),
            case Nonce of
                ExpNonce ->
                    case IsAdmin of
                        user ->
                            debug(Debug, "Admin not required, nonce good~n", []),
                            {MF, Vals};
                        admin ->
                            debug(Debug, "Check admin~n", []),
                            case check_admin(Admins, Id) of
                                {invalid, Error} ->
                                    debug(Debug, "Not admin ~p~n", [Error]),
                                    Error;
                                _ ->
                                    debug(Debug, "Is admin dispatch ~p~n", [MF, Vals]),
                                    {MF, Vals}
                            end
                    end;
                _ ->
                    {{belka_router, '60 (nonce)'}, []}
            end
    end.

% This function walks down the list of administrators and compares the person attempting to access the admin pages one by one.

check_admin([],       _Id) -> {invalid, {{belka_router, '60 (hacker)'}, []}};
check_admin([Id | _T], Id) -> is_admin;
check_admin([_H | T],  Id) -> check_admin(T, Id).

% The design goal here is to keep the route definition as simple and readable as possible so that the developer can easily reason about the plumbing. To that end there is a compiler that simply transforms the readable representation of a route to a more detailed data structure that is more useful to check against.

compile_routes(Routes) -> [compile_route(X) || X <- Routes].

compile_route({Path, {NeedsLogin, IsAdmin}, Dispatch}) ->
    #{path        => compile_path(Path, NeedsLogin),
      needs_login => NeedsLogin,
      is_admin    => IsAdmin,
      dispatch    => Dispatch}.

compile_path(Path, NeedsLogin) ->
    Segs   = string:tokens(Path, "/"),
    KVSegs = [make_seg(X) || X <- Segs],
    case NeedsLogin of
        nonce -> KVSegs ++ [nonce];
        _     -> KVSegs
    end.

make_seg(":" ++ Seg) -> {Seg, []};
make_seg(X)          -> X.

% The requirements for the nonce are threefold:
%
% * its is unique for the path
% * it is unique for the user
% * it is unique for the site
% ^
% so we generate a hash using:
%
% * the path
% * the user's public key
% * a site specific salt
% ^
% We make it URL safe too
make_nonce(URL, #{key := K}, Salt) ->
    Nonce = crypto:hash(md5, list_to_binary([Salt, URL, integer_to_list(K)])),
    SafeNonce = binary:encode_hex(Nonce),
    binary_to_list(SafeNonce).