%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(usr_public_handler).
-behaviour(cowboy_http_handler).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([init/3, handle/2, terminate/2]).

-record(state, {require_login = false :: boolean()}).

-define(ICON_DIR, "/usr/local/var/hivespark/images/").

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
handle(Req, #state{require_login = true} = State) ->
    {ok, Req, State};

handle(Req, #state{require_login = false} = State) ->
    {PathList, _} = cowboy_http_req:path(Req),
    ParamList = hs_util:get_request_params(Req),

    [<<"usr_public">>, Action] = PathList,
    Args = [ParamList, Req, State],

    {StatusCode, H, Bin, NS} = case Action of
                                   <<"new">>    ->  new(Args);
                                   <<"create">> ->  create(Args)
                               end,
                
    {Req2, NewState} = {hs_util:reply(StatusCode, H, Bin, Req), NS},
    {ok, Req2, NewState}.

%%%===================================================================
%%% Request Handle Functions
%%%===================================================================

new([_ParamList, _Req, State]) ->
    hs_util:view("create_user.html", State).

%% 新規ユーザ登録
create([ParamList, _Req, State]) ->
    Name = proplists:get_value(<<"name">>, ParamList),
    LongName = proplists:get_value(<<"longname">>, ParamList),
    Mail = proplists:get_value(<<"email">>, ParamList),
    Password = proplists:get_value(<<"password">>, ParamList),
    IconUrl = proplists:get_value(<<"icon_url">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),
    Format = proplists:get_value(<<"format">>, ParamList),

    Result = hs_usr:create(Name, LongName, Mail, Password, IconUrl, 
                           Description),

    Reply = case Result of
                {ok, Usr} -> 
                    [{<<"result">>, true}, 
                     {<<"usr">>, {hs_usr:to_tuple(Usr)}}];
                {error, Reason} ->
                    ?debugVal(Reason),
                    [{<<"result">>, false}, 
                     {<<"reason">>, list_to_binary(atom_to_list(Reason))}]
            end,

    case Format of
        <<"html">> ->
            hs_util:redirect_to("/auth/index", State);
        _ ->
            hs_util:ok(jiffy:encode({Reply}), State)
    end.
