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
-export([all/4, list/4, checkin/4, add_usr/4, show_checkin/4, 
         send_message/4, get_messages/4]).

%%%===================================================================
%%% API
%%%===================================================================

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
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    TextBin = proplists:get_value(<<"text">>, ParamList),
    Usr = hs_session:get_usr(SessionKey),
    Msg = #message{usr_id = Usr#usr.id, text = TextBin},

    Reply = case hs_team:add_message(TeamId, Msg) of
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
                undefined -> 0;
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

