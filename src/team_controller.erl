%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(team_controller).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/4, update/4, all/4, list/4, checkin/4, add_usr/4, 
         show_checkin/4, send_message/4, get_messages/4, 
         send_article/4, get_articles/4]).

%%%===================================================================
%%% API
%%%===================================================================

create(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    Name = proplists:get_value(<<"name">>, ParamList),
    IconUrl = proplists:get_value(<<"icon_url">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),
    Result = hs_team:create(Name, Usr#usr.id, IconUrl, Description),

    Reply = case Result of
                {ok, Team} -> 
                    [{result, true}, {team, hs_team:to_tuple(Team)}];
                {error, Reason} ->
                    [{result, false}, 
                     {reason, list_to_binary(atom_to_list(Reason))}]
            end,
    {200, jiffy:encode({Reply}), State}.

update(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    Name = proplists:get_value(<<"name">>, ParamList),
    IconUrl = proplists:get_value(<<"icon_url">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),

    case hs_team_db:lookup_id(TeamId) of
        {error, Reason} -> 
            Reply = [{result, false}, 
                     {reason, list_to_binary(atom_to_list(Reason))}],
            {404, jiffy:encode({Reply}), State};
        {ok, Team} ->
            ?debugVal(Team),
            {ok, Owner} = hs_usr:lookup_id(Team#team.owner_id),
            OwnerId = Owner#usr.id,
            UsrId = Usr#usr.id,
            OwnerId = UsrId,
            NewTeam = Team#team{name = Name, icon_url = IconUrl, 
                                description = Description},

            Reply = case hs_team:update(NewTeam) of
                        {ok, T} -> 
                            [{result, true}, {team, hs_team:to_tuple(T)}];
                        {error, Reason} ->
                            [{result, false}, 
                             {reason, list_to_binary(atom_to_list(Reason))}]
                    end,
            {200, jiffy:encode({Reply}), State}
    end.

all(_ParamList, _Req, State, _SessionKey) ->
    Teams = lists:map(fun(Team) -> hs_team:to_tuple(Team) end, 
                      hs_team_db:all()),
    Reply = {[{<<"teams">>, Teams}]},
    {200, jiffy:encode(Reply), State}.    

list(_ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    Teams = lists:map(fun(Team) -> hs_team:to_tuple(Team) end, 
                      hs_usr:get_teams(Usr#usr.id)),
    Reply = {[{<<"teams">>, Teams}]},
    {200, jiffy:encode(Reply), State}.    

add_usr(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),    
    ok = hs_usr:add_team(Usr#usr.id, TeamId),
    Reply = {[{<<"result">>, true}]},
    {200, jiffy:encode(Reply), State}.

checkin(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),

    {ok, Ms} = hs_team:checkin(SessionKey, TeamId, Usr#usr.id),
    Members = lists:map(fun(U) -> hs_usr:to_tuple(U) end, Ms),
    Reply = {[{<<"team_id">>, TeamId}, {<<"members">>, Members}]},
    {200, jiffy:encode(Reply), State}.    

show_checkin(_ParamList, _Req, State, SessionKey) ->
    {ok, TeamId} = hs_session:get_value(SessionKey, "checkin_team_id"),
    {ok, Team} = hs_team:lookup_id(TeamId),

    Reply = {[{<<"team">>, hs_team:to_tuple(Team)}]},
    {200, jiffy:encode(Reply), State}.        

send_message(ParamList, _Req, State, SessionKey) ->
    TeamIdBin = proplists:get_value(<<"team_id">>, ParamList),
    TeamId = list_to_integer(binary_to_list(TeamIdBin)),
    TextBin = proplists:get_value(<<"text">>, ParamList),
    Usr = hs_session:get_usr(SessionKey),
    Msg = #message{usr_id = Usr#usr.id, team_id = TeamId, text = TextBin},

    Reply = case hs_team:add_message(Msg) of
                {ok, Message} ->
                    TMsg = hs_message:to_tuple(Message),
                    {[{<<"result">>, <<"success">>}, {message, TMsg}]};
                {error, Reason} ->
                    {[{<<"result">>, <<"failure">>}, 
                      {<<"reason">>, list_to_binary(atom_to_list(Reason))}]}
            end,    
    {200, jiffy:encode(Reply), State}.    

get_messages(ParamList, _Req, State, _SessionKey) ->
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    Count = case proplists:get_value(<<"count">>, ParamList) of
                undefined -> 40;
                CountBin -> list_to_integer(binary_to_list(CountBin))
            end,
    Offset = case proplists:get_value(<<"offset">>, ParamList) of
                 undefined -> 0;
                 OffsetBin -> list_to_integer(binary_to_list(OffsetBin))
             end,

    Reply = case hs_team:get_messages(TeamId, Offset, Count) of
                {error, not_found} ->
                    {[{<<"error">>, <<"not_found">>}]};
                {ok, MessageList} ->
                    lists:map(fun(Msg) -> hs_message:to_tuple(Msg) end,
                              MessageList)
            end,
    {200, jiffy:encode(Reply), State}.

send_article(ParamList, _Req, State, SessionKey) ->
    TeamIdBin = proplists:get_value(<<"team_id">>, ParamList),
    TeamId = list_to_integer(binary_to_list(TeamIdBin)),
    TextBin = proplists:get_value(<<"text">>, ParamList),
    TitleBin = proplists:get_value(<<"title">>, ParamList),
    Usr = hs_session:get_usr(SessionKey),
    Art = #article{usr_id = Usr#usr.id, team_id = TeamId, text = TextBin, title = TitleBin},

    Reply = case hs_team:add_article(Art) of
                {ok, Article} ->
                    TArt = hs_article_db:to_tuple(Article),
                    {[{<<"result">>, true}, {article, TArt}]};
                {error, Reason} ->
                    {[{<<"result">>, <<"failure">>}, 
                      {<<"reason">>, list_to_binary(atom_to_list(Reason))}]}
            end,    
    {200, jiffy:encode(Reply), State}.    

get_articles(ParamList, _Req, State, _SessionKey) ->
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    Count = case proplists:get_value(<<"count">>, ParamList) of
                undefined -> 40;
                CountBin -> list_to_integer(binary_to_list(CountBin))
            end,
    Offset = case proplists:get_value(<<"offset">>, ParamList) of
                 undefined -> 0;
                 OffsetBin -> list_to_integer(binary_to_list(OffsetBin))
             end,

    Reply = case hs_team:get_articles(TeamId, Offset, Count) of
                {error, not_found} ->
                    {[{<<"error">>, <<"not_found">>}]};
                {ok, ArticleList} ->
                    lists:map(fun(A) -> hs_article_db:to_tuple(A) end,
                              ArticleList)
            end,
    {200, jiffy:encode(Reply), State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

