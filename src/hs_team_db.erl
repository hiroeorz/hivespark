%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_team_db).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([q/1, q/2,
         all/0, insert/4, list/1, statuses_list/2, 
         lookup_id/1, lookup_name/1, 
         update/1, delete/1]).

-define(KEY_PHRASE_1, "message_box3").
-define(KEY_PHRASE_2, "SHIMANE").
-define(KEY_PHRASE_3, "MATSUE").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc exec sql query.
%% @end
%%--------------------------------------------------------------------
-spec q(Sql) -> ok | {ok, [#usr{}]} | {error, Reason} when
      Sql :: string(),
      Reason :: tuple().
q(Sql) -> q(Sql, []).

-spec q(Sql, Params) -> ok | {ok, [#usr{}]} | {error, Reason} when
      Sql :: string(),
      Params :: [any()],
      Reason :: tuple().
q(Sql, Params) ->
    case postgres_pool:equery(?DB, Sql, Params) of
        {ok, Columns, Values} -> 
            {ok, parse_result(Columns, Values, [])};
        {ok, _Count} -> 
            ok;
        {ok, _Count, Columns, Values} -> 
            {ok, parse_result(Columns, Values, [])};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc get all team list.
%% @end
%%--------------------------------------------------------------------
-spec all() -> [TeamList] when
      TeamList :: [] | [#team{}].
all() ->
    {ok, TeamList} = q("select * from teams order by id desc"),
    TeamList.

%%--------------------------------------------------------------------
%% @doc get team list of given status.
%% @end
%%--------------------------------------------------------------------
-spec statuses_list(Lebel, Count) -> [TeamList] when
      Lebel :: integer(),
      Count :: integer(),
      TeamList :: [] | [#team{}].
statuses_list(Level, Count) ->
    {ok, TeamList} = q("select * from teams where status = $1
                          order by id desc limit $2", [Level, Count]),
    TeamList.    

%%--------------------------------------------------------------------
%% @doc get team list from id list.
%% @end
%%--------------------------------------------------------------------
-spec list(TeamIdList) -> {ok, TeamList} when
      TeamIdList :: [integer() | string() | binary()],
      TeamList :: [] | [#team{}].
list([]) -> {ok, []};

list([TeamId | _] = TeamIdList) when is_integer(TeamId) ->
    list(lists:map(fun(Id) -> list_to_binary(integer_to_list(Id)) end, 
                   TeamIdList));

list([TeamId | _] = TeamIdList) when is_binary(TeamId) ->
    list(lists:map(fun(Id) -> binary_to_list(Id) end, 
                   TeamIdList));

list([TeamId | _] = TeamIdList) when is_list(TeamId) ->
    Sql = lists:flatten(io_lib:format("select * from teams where id in (~s)",
                                      [string:join(TeamIdList, ",")])),
    case q(Sql) of
        {ok, Records} -> {ok, Records};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc insert new team to database.
%% @end
%%--------------------------------------------------------------------
-spec insert(Name, OwnerId, IconUrl, Description) -> 
                    {ok, Team} | {error, Reason} when
      Name :: binary(),
      OwnerId :: integer(),
      IconUrl :: binary(),
      Description :: binary(),
      Team :: #team{},
      Reason :: atom().
insert(Name, OwnerId, IconUrl, Description) ->
    CreatedAt = {date(), time()},
    Result = q("insert into teams (name, owner_id, icon_url, description, 
                                   created_at)
                  values($1, $2, $3, $4, $5)
                  returning *",
               [Name, OwnerId, IconUrl, Description, CreatedAt]),

    case Result of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(TeamId) -> {ok, Team} | {error, not_found} |{error, Reason} when
      TeamId :: integer() | list() | binary(),
      Team :: #team{},
      Reason :: tuple().
lookup_id(TeamId) when is_binary(TeamId) -> lookup_id(binary_to_list(TeamId));
lookup_id(TeamId) when is_list(TeamId) -> lookup_id(list_to_integer(TeamId));
lookup_id(TeamId) when is_integer(TeamId) -> 
    case q("select * from teams where id = $1", [TeamId]) of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.        

%%--------------------------------------------------------------------
%% @doc lookup user by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Team} | {error, not_found} | {error, Reason} when
      Name :: string(),
      Team :: #team{},
      Reason :: tuple().
lookup_name(Name) when is_binary(Name) or is_list(Name) ->
    case q("select * from teams where name = $1", [Name]) of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.        

%%--------------------------------------------------------------------
%% @doc update user parameter.
%% @end
%%--------------------------------------------------------------------
-spec update(Team) -> {ok, UpdatedTeam} | {error, Reason} when
      Team :: #team{},
      UpdatedTeam :: #team{},
      Reason :: atom().
update(Team) ->
    Result = q("update teams set name = $2,
                             icon_url = $3,
                             description = $4,
                             status_description = $5,
                             status = $6
                  where id = $1
                  returning *",
              [Team#team.id, Team#team.name, Team#team.icon_url, 
               Team#team.description, Team#team.status_description,
               Team#team.status]),

    case Result of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc delete usr from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(TeamId) -> {ok, deleted} | {error, Reason} when
      TeamId :: integer() | string() | binary(),
      Reason :: tuple().
delete(TeamId) when is_binary(TeamId) -> delete(binary_to_list(TeamId));
delete(TeamId) when is_list(TeamId) -> delete(list_to_integer(TeamId));
delete(TeamId) when is_integer(TeamId) ->
    case q("delete from teams where id = $1", [TeamId]) of
        ok -> {ok, deleted};
        {error, Reason} -> {error, Reason}
    end.     

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc sql query result parser.
%% @end
%%--------------------------------------------------------------------
-spec parse_result(Columns, Records, []) -> ParsedResults when
      Columns :: [tuple()],
      Records :: [tuple()],
      ParsedResults :: [tuple()].
parse_result(_Columns, [], Results) -> lists:reverse(Results);
parse_result(Columns, [DBRecord | RTail], Results) ->
    Record = parse_record(Columns, tuple_to_list(DBRecord), #team{}),
    parse_result(Columns, RTail, [Record | Results]).

-spec parse_record(Columns, Values, EmptyRecord) -> Record when
      Columns :: [tuple()],
      Values :: [tuple()],
      EmptyRecord :: #team{},
      Record :: #team{}.
parse_record([], [], Result) -> Result;
parse_record([Column | CTail], [Value | VTail], Result) ->
    {column, Name, _, _, _, _} = Column,
    Result1 = case Name of
                  <<"id">> -> Result#team{id = Value};
                  <<"name">> -> Result#team{name = Value};
                  <<"owner_id">> -> Result#team{owner_id = Value};
                  <<"icon_url">> -> Result#team{icon_url = Value};
                  <<"status">> -> Result#team{status = Value};
                  <<"status_description">> -> 
                      Result#team{status_description = Value};
                  <<"description">> -> Result#team{description = Value};
                  <<"created_at">> -> 
                      case Value of
                          undefined -> undefined;
                          V -> 
                              Result#team{
                                created_at = hs_util:pgdatetime_to_seconds(V)}
                      end
              end,
    parse_record(CTail, VTail, Result1).

