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
-export([index/4, new/4, show/4, edit/4, upload_icon/4, delete/4,
         info/4, create/4, update/4, all/4, list/4, statuses_list/4,
         checkin/4, add_usr/4, delete_usr/4,
         show_checkin/4, send_message/4, get_messages/4, 
         save_image/4, image/4]).

-define(ICON_DIR, "/usr/local/var/hivespark/images/").

%%%===================================================================
%%% API
%%%===================================================================

index(_ParamList, _Req, State, _SessionKey) ->
    hs_util:view("team_index.html", State).

new(_ParamList, _Req, State, _SessionKey) ->
    hs_util:view("team_new.html", State).

show(_ParamList, _Req, State, _SessionKey) ->
    hs_util:view("team_show.html", State).

edit(_ParamList, _Req, State, _SessionKey) ->
    hs_util:view("team_edit.html", State).

upload_icon(_ParamList, _Req, State, _SessionKey) ->
    hs_util:view("team_upload_icon.html", State).

info(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    case hs_team:lookup_id(TeamId) of
        {ok, Team} -> 
            ?debugVal(Team),
            Reply = {[{team, hs_team:to_tuple(Team)},
                      {is_member, hs_team:is_member(Team#team.id, Usr#usr.id)},
                      {is_owner, hs_team:is_owner(Team#team.id, Usr#usr.id)} ]},
            ?debugVal(Reply),
            hs_util:ok(jiffy:encode(Reply), State);
        {error, not_found} -> 
            hs_util:not_found(jiffy:encode([{error, not_found}]))
    end.

create(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    Name = proplists:get_value(<<"name">>, ParamList),
    IconUrl = proplists:get_value(<<"icon_url">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),
    Format = proplists:get_value(<<"format">>, ParamList),
    Result = hs_team:create(Name, Usr#usr.id, IconUrl, Description),

    Reply = case Result of
                {ok, Team} -> 
                    [{result, true}, {team, hs_team:to_tuple(Team)}];
                {error, Reason} ->
                    [{result, false}, 
                     {reason, list_to_binary(atom_to_list(Reason))}]
            end,

    case Format of
        <<"html">> ->
            hs_util:redirect_to("/team/index", State);
        _ ->
            hs_util:ok(jiffy:encode({Reply}), State)
    end.
            
update(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    TeamIdStr = binary_to_list(TeamId),                    
    Name = proplists:get_value(<<"name">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),
    StatusDescription = proplists:get_value(<<"status_description">>, 
                                            ParamList),

    StatusBin = proplists:get_value(<<"status">>, ParamList),
    Status = list_to_integer(binary_to_list(StatusBin)),
    ?debugVal(Status),

    Format = proplists:get_value(<<"format">>, ParamList),

    case hs_team_db:lookup_id(TeamId) of
        {error, Reason} -> 
            Reply = [{result, false}, 
                     {reason, list_to_binary(atom_to_list(Reason))}],

            case Format of
                <<"html">> -> 
                    hs_util:not_found(State);
                _ -> 
                    hs_util:not_found(jiffy:encode({Reply}), State)
            end;

        {ok, Team} ->
            case hs_team:is_member(Team#team.id, Usr#usr.id) of
                false -> hs_util:forbidden(State);
                true -> 
                    NewTeam = Team#team{name = Name, description = Description,
                                        status = Status,
                                        status_description = StatusDescription},
            
                    Reply = case hs_team:update(NewTeam) of
                                {ok, T} -> 
                                    [{result, true}, 
                                     {team, hs_team:to_tuple(T)}];
                                {error, Reason} ->
                                    [{result, false}, 
                                     {reason, 
                                      list_to_binary(atom_to_list(Reason))}]
                            end,

                    case Format of
                        <<"html">> -> 
                            Url = "/team/show?team_id=" ++ TeamIdStr,
                            hs_util:redirect_to(Url, State);
                        _ -> 
                            hs_util:ok(jiffy:encode({Reply}), State)
                    end
            end
    end.

all(_ParamList, _Req, State, _SessionKey) ->
    Teams = lists:map(fun(Team) -> hs_team:to_tuple(Team) end, 
                      hs_team_db:all()),
    Reply = {[{<<"teams">>, Teams}]},
    hs_util:ok(jiffy:encode(Reply), State).    

list(_ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    Teams = lists:map(fun(Team) -> hs_team:to_tuple(Team) end, 
                      hs_usr:get_teams(Usr#usr.id)),
    Reply = {[{<<"teams">>, Teams}]},
    hs_util:ok(jiffy:encode(Reply), State).    

statuses_list(_ParamList, _Req, State, _SessionKey) ->
    StopTeams = lists:map(fun(T) -> hs_team:to_tuple(T) end,
                          hs_team:statuses_list(0, 6)),
    BurningTeams = lists:map(fun(T) -> hs_team:to_tuple(T) end,
                             hs_team:statuses_list(1, 6)),

    Reply = {[{<<"stop_teams">>, StopTeams},
              {<<"burning_teams">>, BurningTeams}]},

    hs_util:ok(jiffy:encode(Reply), State).

delete(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamIdBin = proplists:get_value(<<"team_id">>, ParamList),
    TeamId = list_to_integer(binary_to_list(TeamIdBin)),

    case hs_team:is_owner(TeamId, Usr#usr.id) of
        false -> hs_util:forbidden(State);
        true -> 
            case hs_team:delete(TeamId) of
                {error, not_found} -> hs_util:not_found(State);
                {ok, deleted} -> 
                    {ok, deleted} = hs_usr_team_db:delete_teams_usrs(TeamId),
                    Reply = {[{<<"result">>, true}]},
                    hs_util:ok(jiffy:encode(Reply), State)
            end
    end.

add_usr(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamIdBin = proplists:get_value(<<"team_id">>, ParamList),
    TeamId = list_to_integer(binary_to_list(TeamIdBin)),
    ok = hs_usr:add_team(Usr#usr.id, TeamId),
    Reply = {[{<<"result">>, true}]},
    hs_util:ok(jiffy:encode(Reply), State).

delete_usr(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamIdBin = proplists:get_value(<<"team_id">>, ParamList),
    TeamId = list_to_integer(binary_to_list(TeamIdBin)),
    ok = hs_usr:delete_team(Usr#usr.id, TeamId),
    Reply = {[{<<"result">>, true}]},
    hs_util:ok(jiffy:encode(Reply), State).

checkin(ParamList, _Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),

    {ok, Ms} = hs_team:checkin(SessionKey, TeamId, Usr#usr.id),
    Members = lists:map(fun(U) -> hs_usr:to_tuple(U) end, Ms),
    Reply = {[{<<"team_id">>, TeamId}, {<<"members">>, Members}]},
    hs_util:ok(jiffy:encode(Reply), State).    

show_checkin(_ParamList, _Req, State, SessionKey) ->
    {ok, TeamId} = hs_session:get_value(SessionKey, "checkin_team_id"),
    {ok, Team} = hs_team:lookup_id(TeamId),

    Reply = {[{<<"team">>, hs_team:to_tuple(Team)}]},
    hs_util:ok(jiffy:encode(Reply), State).        

send_message(ParamList, _Req, State, SessionKey) ->
    TeamIdBin = proplists:get_value(<<"team_id">>, ParamList),
    TeamId = list_to_integer(binary_to_list(TeamIdBin)),
    TextBin = proplists:get_value(<<"text">>, ParamList),
    Usr = hs_session:get_usr(SessionKey),
    Msg = #message{usr_id = Usr#usr.id, team_id = TeamId, text = TextBin},

    Reply = case hs_team:add_message(Msg) of
                {ok, Message} ->
                    TMsg = hs_message:to_tuple(Message),
                    {[{<<"result">>, true}, {message, TMsg}]};
                {error, Reason} ->
                    ?debugVal(Reason),
                    {[{<<"result">>, <<"failure">>}, 
                      {<<"reason">>, list_to_binary(atom_to_list(Reason))}]}
            end,    
    hs_util:ok(jiffy:encode(Reply), State).    

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
    hs_util:ok(jiffy:encode(Reply), State).

image(ParamList, _Req, State, _SessionKey) ->
    FName = proplists:get_value(<<"fname">>, ParamList),
    Path = lists:flatten([?ICON_DIR, binary_to_list(FName)]),

    case file:read_file(Path) of
        {ok, Binary} -> hs_util:ok(Binary, State);
        {error, enoent} -> hs_util:not_found("image not found", State)
    end.

save_image(_, Req, State, _SessionKey) ->
    {Results, _} = hs_util:acc_multipart(Req, []),
    ParamList = hs_util:get_param_data(Results),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    Format = proplists:get_value(<<"format">>, ParamList),
    {ok, Team} = hs_team:lookup_id(TeamId),

    Result = case hs_util:get_multi_data(Results) of
                 {error, not_found} -> error;
                 {ok, Data, Type} ->
                     FName = "team_" ++ integer_to_list(Team#team.id),
                     Ext = hs_util:ext_part(Type),
                     Path = lists:flatten([?ICON_DIR, FName, Ext]),
                     ok = file:write_file(Path, Data),

                     IconUrlStr = lists:flatten(["/team/image?fname=", 
                                                 FName, Ext]),
                     IconUrl = list_to_binary(IconUrlStr),
                     {ok, _} = hs_team:update(Team#team{icon_url = IconUrl}),
                     ok
             end,

    Reply = case Result of
                ok -> {[{result, <<"success">>}]};
                _ -> {[{result, <<"failure">>}]}
            end,

    TeamIdStr = binary_to_list(TeamId),                    
    case Format of
        <<"html">> -> 
            Url = "/team/show?team_id=" ++ TeamIdStr,
            hs_util:redirect_to(Url, State);
        _ -> 
            hs_util:ok(jiffy:encode({Reply}), State)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

