%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_usr).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/6, update/1, delete/1, lookup_id/1, lookup_name/1, 
         authenticate/2, 
         get_teams/1, add_team/2, delete_team/2, 
         to_tuple/1, add_message/2, get_messages/2, get_messages/3,
         checkin_to_team/2, get_checkin_team/1]).

-define(USR_TIMELINE, <<"_hut_">>).
-define(USR_CHECKIN_TEAM, <<"hs_usr_checkin_team">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create new user.
%% @end
%%--------------------------------------------------------------------
-spec create(Name, LongName, Mail, Password, IconUrl, Description) -> 
                    {ok, Usr} | {error, Reason} when
      Name :: binary(),
      LongName :: binary(),
      Mail :: binary(),
      Password :: binary(),
      IconUrl :: binary(),
      Description :: binary(),
      Usr :: #usr{},
      Reason :: atom().
create(Name, LongName, Mail, Password, IconUrl, Description) ->
    case hs_usr_db:lookup_name(Name) of
        {ok, _Usr} ->
            {error, already_exist};
        {error, not_found} ->
            hs_usr_db:insert(Name, LongName, Mail, Password, 
                                    IconUrl, Description)
    end.

%%--------------------------------------------------------------------
%% @doc update user.
%% @end
%%--------------------------------------------------------------------
-spec update(Usr) -> {ok, NewUsr} | {error, not_found} when
      Usr :: #usr{},
      NewUsr :: #usr{}.
update(Usr) ->
    case hs_usr:lookup_id(Usr#usr.id) of
        {error, not_found} -> {error, not_found};
        {ok, _} -> 
            hs_usr_cache:delete(Usr#usr.id),
            hs_usr_db:update(Usr)
    end.

%%--------------------------------------------------------------------
%% @doc delete user.
%% @end
%%--------------------------------------------------------------------
-spec delete(UsrId) -> {ok, deleted} | {error, not_found} when
      UsrId :: integer() | string() | binary().
delete(UsrId) ->
    case hs_usr_db:lookup_id(UsrId) of
        {error, not_found} ->
            {error, not_found};
        {ok, _Usr} ->
            hs_usr_cache:delete(UsrId),
            {ok, deleted} = hs_usr_db:delete(UsrId),
            {ok, deleted}
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(UsrId) -> {ok, Usr} | {error, not_found} when
      UsrId :: integer() | list() | binary(),
      Usr :: #usr{}.
lookup_id(UsrId) ->
    case hs_usr_cache:lookup_id(UsrId) of
        {ok, Usr} -> {ok, Usr};
        {error, not_found} ->
            case hs_usr_db:lookup_id(UsrId) of
                {error, not_found} -> {error, not_found};
                {ok, Usr} -> 
                    hs_usr_cache:store(Usr),
                    {ok, Usr}
            end
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Usr} | {error, not_found} when
      Name :: string(),
      Usr :: #usr{}.
lookup_name(Name) ->
    case hs_usr_cache:lookup_name(Name) of
        {ok, Usr} -> {ok, Usr};
        {error, not_found} ->
            case hs_usr_db:lookup_name(Name) of
                {error, not_found} -> {error, not_found};
                {ok, Usr} -> 
                    hs_usr_cache:store(Usr),
                    {ok, Usr}
            end
    end.

-spec authenticate(UsernameBin, PasswordBin) -> {ok, UsrId} | failure when
      UsernameBin :: binary(),
      PasswordBin :: binary(),
      UsrId :: integer().
authenticate(UsernameBin, PasswordBin) ->
    hs_usr_db:authenticate(UsernameBin, PasswordBin).

-spec get_teams(UsrId) -> [#team{}] when
      UsrId :: integer() | binary().
get_teams(UsrId) ->
    TeamIds = case hs_usr_cache:get_team_id_list(UsrId) of
                [] -> 
                    IdsFromDb = hs_usr_db:get_team_id_list(UsrId),
                    ok = hs_usr_cache:add_team_id_list(UsrId, IdsFromDb),
                    IdsFromDb;
                TeamIds1 -> TeamIds1
            end,

    hs_team_db:list(TeamIds).

-spec add_team(UsrId, TeamId) -> ok when
      UsrId :: integer() | binary(),
      TeamId :: integer() | binary().
add_team(UsrId, TeamId) -> 
    hs_usr_cache:delete_team_id_list(UsrId),
    hs_usr_db:add_team(UsrId, TeamId).

-spec delete_team(UsrId, TeamId) -> ok when
      UsrId :: integer() | binary(),
      TeamId :: integer() | binary().      
delete_team(UsrId, TeamId) ->
    hs_usr_cache:delete_team_id_list(UsrId),
    hs_usr_db:delete_team(UsrId, TeamId).
    
-spec checkin_to_team(UsrId, TeamId) -> ok when
      UsrId :: integer(),
      TeamId :: integer().
checkin_to_team(UsrId, TeamId) ->
    {ok, _} = eredis_pool:q(?DB_SRV, 
                            ["HSET", ?USR_CHECKIN_TEAM, UsrId, TeamId]),
    ok.

-spec get_checkin_team(UsrId) -> {ok, Team} | 
                                 {error, not_found} | 
                                 {error, not_checkin_user} when
      UsrId :: integer(),
      Team :: #team{}.
get_checkin_team(UsrId) ->
    case eredis_pool:q(?DB_SRV, ["HGET", ?USR_CHECKIN_TEAM, UsrId]) of
        {ok, undefined} -> {error, not_checkin_user};
        {ok, TeamId} -> 
            case hs_team:lookup_id(TeamId) of
                {error, not_found} -> {error, not_found};
                {ok, Team} -> {ok, Team}
            end
    end.
                    
%%--------------------------------------------------------------------
%% @doc parse usr for json object.
%% @end
%%--------------------------------------------------------------------
-spec to_tuple(Usr) -> TupleUsr when
      Usr :: #usr{} | integer(),
      TupleUsr :: tuple().
to_tuple(UsrId) when is_integer(UsrId) ->
    case lookup_id(UsrId) of
        {error, Reason} -> {error, Reason};
        {ok, Usr} -> {ok, to_tuple(Usr)}
    end;

to_tuple(Usr) ->
    {[{id, Usr#usr.id}, {name, Usr#usr.name}, {longname, Usr#usr.longname},
      {icon_url, Usr#usr.icon_url}, {lat, Usr#usr.lat}, {lng, Usr#usr.lng},
      {description, Usr#usr.description}]}.

%%--------------------------------------------------------------------
%% @doc add message to users timeline.
%% @end
%%--------------------------------------------------------------------
-spec add_message(UsrId, MsgId) -> ok when
      UsrId :: integer(),
      MsgId :: binary().
add_message(UsrId, MsgId) when is_integer(UsrId), is_binary(MsgId) ->
    Key = get_key_of_timeline(UsrId),
    {ok, _} = eredis_pool:q(?DB_SRV, ["LPUSH", Key, MsgId]),
    ok.

%%--------------------------------------------------------------------
%% @doc add message to users timeline.
%% @end
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc add message to users timeline.
%% @end
%%--------------------------------------------------------------------
-spec get_messages(UsrId, Count) -> 
                          {ok, MessageList} | {error, not_found}  when
      UsrId :: integer(),
      Count :: integer(),
      MessageList :: [#message{}] | [].
get_messages(UsrId, Count) -> 
    get_messages(UsrId, 0, Count).

-spec get_messages(UsrId, Offset, Count) -> 
                          {ok, MessageList} | {error, not_found}  when
      UsrId :: integer(),
      Offset :: integer(),
      Count :: integer(),
      MessageList :: [#message{}] | [].
get_messages(UsrId, Offset, Count) ->
    case get_message_ids(UsrId, Offset, Count) of
        {ok, undefined} -> {error, not_found};
        {ok, Ids} -> {ok, hs_message:mget_msg(Ids)}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_key_of_timeline(UsrId) -> Key when
      UsrId :: integer(),
      Key :: binary().
get_key_of_timeline(UsrId) ->
    list_to_binary(lists:flatten([?USR_TIMELINE, integer_to_list(UsrId)])).

-spec get_message_ids(UsrId, Offset, Count) -> 
                          {ok, MsgIdList} | {error, not_found} when
      UsrId :: integer(),
      Offset :: integer(),
      Count :: integer(),
      MsgIdList :: [binary()].
get_message_ids(UsrId, Offset, Count) ->
    StartPos = 0 - Offset - 1,
    EndPos = 0 - Count,
    Key = get_key_of_timeline(UsrId),

    case eredis_pool:q(?DB_SRV, ["LRANGE", Key, EndPos, StartPos]) of
        {ok, undefined} -> {error, not_found};
        {ok, MsgIdList} -> {ok, MsgIdList}
    end.
