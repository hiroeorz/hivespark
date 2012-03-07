%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 23 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(usr_public_controller).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% 新規ユーザ登録
create(ParamList, _Req, State) ->
    Name = proplists:get_value(<<"name">>, ParamList),
    LongName = proplists:get_value(<<"longname">>, ParamList),
    Mail = proplists:get_value(<<"email">>, ParamList),
    Password = proplists:get_value(<<"password">>, ParamList),
    IconUrl = proplists:get_value(<<"icon_url">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),

    Result = hs_usr:create(Name, LongName, Mail, Password, IconUrl, 
                                  Description),

    Reply = case Result of
                {ok, Usr} -> 
                    [{<<"result">>, true}, 
                     {<<"usr">>, {hs_usr:to_tuple(Usr)}}];
                {error, Reason} ->
                    [{<<"result">>, false}, 
                     {<<"reason">>, list_to_binary(atom_to_list(Reason))}]
            end,

    {200, [], jiffy:encode({Reply}), State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
