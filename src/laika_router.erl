%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

%%%-------------------------------------------------------------------
%% @doc router
%%
%% the router is used to match incoming Gemini URLs to resources
%% it picks up its routes from the modules routes.erl
%%
%% @end
%%%-------------------------------------------------------------------

-module(laika_router).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% API
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         dispatch/1,
         '51'/2,
         '60'/2,
         '60 (nonce)'/2,
         '60 (hacker)'/2,
         make_nonce/3
         ]).

-record(state, {routes = [], salt = "", admins = []}).

% API

% see: http://erlang.org/doc/man/gen_server.html#start_link-3
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

dispatch(Route) ->
    {{M, F}, Vals} = gen_server:call(?MODULE, {route, Route}),
    apply(M, F, [Route, Vals]).

% Callbacks

'51'(Route, Vals) ->
    io:format("in 51 Route is ~p Vals is ~p~n", [Route, Vals]),
    [<<"51 Welcome to Area 51 👽\r\n"/utf8>>].

'60'(Route, Vals)->
    io:format("in 60 Route is ~p Vals is ~p~n", [Route, Vals]),
    [<<"60 Criminal Code Section 60 Violation 👮\r\n"/utf8>>].

'60 (nonce)'(Route, Vals)->
    io:format("in 60 nonce Route is ~p Vals is ~p~n", [Route, Vals]),
    [<<"60 Criminal Code Section 60 Violation - you're goin to jail ya nonce 🚓\r\n"/utf8>>].

'60 (hacker)'(Route, Vals)->
    io:format("in 60 hacker Route is ~p Vals is ~p~n", [Route, Vals]),
    [<<"60 Criminal Code Section 60 Violation - back off hacker ☠️\r\n"/utf8>>].

init(_Args) ->
    {ok, {M, F}} = application:get_env(routes, salt),
    {ok, Salt}   = application:get_env(laika_router, salt),
    {ok, Admins} = application:get_env(laika_router, admins),
    true = register(?MODULE, self()),
    Routes = apply(M, F, []),
    CompiledRoutes = compile_routes(Routes),
    {ok, #state{routes = CompiledRoutes,
                salt   = Salt,
                admins = Admins}}.

handle_call({route, Route}, _From, State) ->
    Routes = State#state.routes,
    Salt = State#state.salt,
    Admins = State#state.admins,
    Reply = get_dispatch(Route, Routes, Salt, Admins),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

get_dispatch(Route, Routes, Salt, Admins) ->
    match_route(Routes, Route, Salt, Admins).

match_route([], _, _Salt, _Admins) -> {{router, '51'}, []};
match_route([H | T], Route, Salt, Admins) ->
    #{path := GotPath, id := Id} = Route,
    #{path := ExpPath, needs_login := Login, is_admin := IsAdmin, dispatch := MF} = H,
    case match_path(GotPath, ExpPath, []) of
        no_match  ->
            match_route(T, Route, Salt, Admins);
        {check_nonce, Vals} ->
            handle_nonce_check(GotPath, Id, MF, Vals, Salt, IsAdmin, Admins);
        {match, Vals} ->
            case {Id, Login, IsAdmin} of
                {no_identity, login, _} ->
                    {{router, '60'}, []};
                {_, login, admin} ->
                    case check_admin(Admins, Id) of
                        {invalid, Error} ->
                            Error;
                        _ ->
                            {MF, Vals}
                    end;
                {_, _, user} ->
                    {MF, Vals}
            end
    end.

handle_nonce_check(GotPath, Id, MF, Vals, Salt, IsAdmin, Admins) ->
    case Id of
        no_identity ->
            {{router, '60'}, []};
        _ ->
            [Nonce | Rest] = lists:reverse(GotPath),
            OrigPath = string:join(lists:reverse(Rest), "/"),
            ExpNonce = make_nonce(OrigPath, Id, Salt),
            case Nonce of
                ExpNonce ->
                    case IsAdmin of
                        user ->
                            {MF, Vals};
                        admin ->
                            case check_admin(Admins, Id) of
                                {invalid, Error} ->
                                    Error;
                                _ ->
                                    {MF, Vals}
                            end
                    end;
                _ ->
                    {{router, '60 (nonce)'}, []}
            end
    end.


check_admin([],       _Id) -> {invalid, {{router, '60 (hacker)'}, []}};
check_admin([Id | _T], Id) -> is_admin;
check_admin([_H | T],  Id) -> check_admin(T, Id).

match_path([],        [],                Acc) -> {match, Acc};
match_path([H | T1],  [H | T2],          Acc) -> match_path(T1, T2, Acc);
match_path([H | T1],  [{Name, []} | T2], Acc) -> match_path(T1, T2, [{Name, H} | Acc]);
match_path([_H | []], [nonce | []],      Acc) -> {check_nonce, Acc};
match_path(_,         _,                _Acc) -> no_match.

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

make_nonce(URL, #{key := K}, Salt) ->
    Nonce = crypto:hash(md5, list_to_binary([Salt, URL, integer_to_list(K)])),
    SafeNonce = binary:encode_hex(Nonce),
    binary_to_list(SafeNonce).