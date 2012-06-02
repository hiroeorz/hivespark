%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(auth_handler).
-behaviour(cowboy_http_handler).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([init/3, handle/2, terminate/2, handle_route/2]).

-record(state, {require_login = false :: boolean()}).

%%%===================================================================
%% @doc HTTP CallBacks
%%%===================================================================

init({tcp, http}, Req, Options) ->
    case lists:member(require_login, Options) of
        true ->
            {ok, Req, #state{require_login = true}};
        false ->
            {ok, Req, #state{require_login = false}}
    end.

terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% @doc HTTP Handler
%%%===================================================================
handle(Req, State) -> hs_router:handle(?MODULE, Req, State).

handle_route(Action, Args) ->
    ?debugVal(Action),
    case Action of
        <<"index">> -> index(Args);
        <<"login">> -> login(Args)
    end.

%%%===================================================================
%%% Request Handle Functions
%%%===================================================================

index([_ParamList, _Req, State, _SessionKey]) ->
    hs_util:view("login.html", State).

login([ParamList, _Req, State, _SessionKey]) ->
    Username = proplists:get_value(<<"username">>, ParamList),
    Password = proplists:get_value(<<"password">>, ParamList),
    Format = proplists:get_value(<<"format">>, ParamList),
    io:format("username:~s password:~s", [Username, Password]),
    Auth = hs_usr:authenticate(Username, Password),

    case Auth of
        {ok, UsrId} ->
            {ok, Usr} = hs_usr:lookup_id(UsrId),
            {ok, SessionKey} = hs_session:create(Usr),
            RepText = lists:flatten(["ようこそ ", Usr#usr.longname, "さん！"]),

            Reply = [{status, true}, {message, list_to_binary(RepText)}, 
                     {usr_id, Usr#usr.id}, {session_key, SessionKey}],

            Cookie1 = cowboy_cookies:cookie(<<"session_key">>, SessionKey,
                                           [{path, <<"/">>}]),

            UsrIdBin = list_to_binary(integer_to_list(UsrId)),
            Cookie2 = cowboy_cookies:cookie(<<"usr_id">>, UsrIdBin,
                                           [{path, <<"/">>}]),
            
            case Format of
                <<"html">> ->
                    hs_util:redirect_to("/team/index", [Cookie1, Cookie2], State);
                _ ->
                    hs_util:ok([Cookie1, Cookie2], jiffy:encode({Reply}), State)
            end;
        Else ->
            ?debugVal(Else),
            Reply = [{status, false}, {message, list_to_binary("認証失敗")}],

            case Format of
                <<"html">> ->
                    {200, _, Html, _} = hs_util:view("login.html", State),
                    hs_util:not_authenticated([], Html, State);
                _ ->
                    hs_util:not_authenticated(jiffy:encode({Reply}), State)
            end
    end.
