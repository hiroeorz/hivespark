%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(team_public_controller).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% 新規チーム登録
create(ParamList, _Req, State) ->
    Name = proplists:get_value(<<"name">>, ParamList),
    IconUrl = proplists:get_value(<<"icon_url">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),
    Result = hs_team:create(Name, IconUrl, Description),

    Reply = case Result of
                {ok, Team} -> 
                    [{result, true}, {team, hs_team:to_tuple(Team)}];
                {error, Reason} ->
                    [{result, false}, 
                     {reason, list_to_binary(atom_to_list(Reason))}]
            end,
    {200, [], jiffy:encode({Reply}), State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
