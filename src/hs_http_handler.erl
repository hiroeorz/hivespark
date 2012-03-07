%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(hs_http_handler).
-behaviour(cowboy_http_handler).

%% Include
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([init/3, handle/2, handle/5, terminate/2]).
-export([reply/4, reply/2]).

%%%===================================================================
%% @doc HTTP CallBacks
%%%===================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.

terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% @doc HTTP Handler
%%%===================================================================
handle(Req, State) ->
    {PathList, _} = cowboy_http_req:path(Req),
    {ParamList, _} = cowboy_http_req:qs_vals(Req),

    case PathList of
        [Controller, Action | _] ->
            handle(Controller, Action, ParamList, Req, State);
        [Controller] ->
            handle(Controller, <<"index">>, ParamList, Req, State);
        [] ->
            handle(<<"top">>, <<"index">>, [], Req, State)
    end.

%%%==================================================================
%%% HTTP Handler
%%%===================================================================
-spec handle(Controller, Action, ParamList, Req, State) -> 
                    {ok, Req2, NewState} when
      Controller :: binary(),
      Action :: binary(),
      ParamList :: [tuple()],
      Req :: tuple(),
      State :: tuple(),
      Req2 :: tuple(),
      NewState :: tuple().

handle(C, A, ParamList, Req, State) when ?AUTHENTICATED_ROUTE ->
    {Cookies, _} = cowboy_http_req:cookies(Req),
    ?debugVal(Cookies),

    UsrId = proplists:get_value(<<"usr_id">>, Cookies),
    SessionKey = proplists:get_value(<<"session_key">>, Cookies),
    Loggedin = hs_session:check_loggedin(UsrId, SessionKey),

    {Req2, NewState} = 
        case Loggedin of
            true -> 
                M = list_to_atom(lists:flatten([binary_to_list(C), 
                                                "_controller"])),
                F = list_to_atom(binary_to_list(A)),
                {StatusCode, Bin, NS} = M:F(ParamList, Req, State, SessionKey),
                {reply(StatusCode, [], Bin, Req), NS};
            false ->
                {reply(401, Req), State}
        end,
    {ok, Req2, NewState};

handle(C, A, ParamList, Req, State) when ?ROUTE ->
    M = list_to_atom(lists:flatten([binary_to_list(C), "_controller"])),
    F = list_to_atom(binary_to_list(A)),
    {StatusCode, H, Bin, NewState} = M:F(ParamList, Req, State),
    Req2 = reply(StatusCode, H, Bin, Req),
    {ok, Req2, NewState};

handle(_, _, _, Req, State) ->
    Req2 = reply(404, Req),
    {ok, Req2, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reply(Status, Headers, Body, Req) ->
    {ok, Req2} = cowboy_http_req:reply(Status, Headers, Body, Req),
    Req2.

reply(Status, Req) ->
    {ok, Req2} = cowboy_http_req:reply(Status, Req),
    Req2.

