%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_teamfile_db).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([q/1, q/2,
         insert/4, lookup_id/1, delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc exec sql query.
%% @end
%%--------------------------------------------------------------------
-spec q(Sql) -> ok | {ok, [#teamfile{}]} | {error, Reason} when
      Sql :: string(),
      Reason :: tuple().
q(Sql) -> q(Sql, []).

-spec q(Sql, Params) -> ok | {ok, [#teamfile{}]} | {error, Reason} when
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
%% @doc insert new team to database.
%% @end
%%--------------------------------------------------------------------
-spec insert(TeamId, OwnerId, Name, Description) ->  
                    {ok, TeamFile} | {error, Reason} when
      TeamId :: integer(),
      OwnerId :: integer(),
      Name :: binary(),
      Description :: binary(),
      TeamFile :: #teamfile{},
      Reason :: atom().
insert(TeamId, OwnerId, Name, Description) ->
    CreatedAt = {date(), time()},
    Result = q("insert into teamfiles (team_id, name, owner_id, description, 
                                       created_at, updated_at)
                  values($1, $2, $3, $4, $5, $6)
                  returning *",
               [TeamId, Name, OwnerId, Description, CreatedAt, CreatedAt]),

    case Result of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(TeamFileId) -> {ok, Team} | {error, not_found} |{error, Reason} when
      TeamFileId :: integer() | list() | binary(),
      Team :: #team{},
      Reason :: tuple().
lookup_id(TeamFileId) when is_binary(TeamFileId) -> 
    lookup_id(binary_to_list(TeamFileId));

lookup_id(TeamFileId) when is_list(TeamFileId) -> 
    lookup_id(list_to_integer(TeamFileId));

lookup_id(TeamFileId) when is_integer(TeamFileId) -> 
    case q("select * from teamfiles where id = $1", [TeamFileId]) of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc delete teamfile from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(TeamFileId) -> {ok, deleted} | {error, Reason} when
      TeamFileId :: integer() | string() | binary(),
      Reason :: tuple().
delete(TeamFileId) when is_binary(TeamFileId) -> 
    delete(binary_to_list(TeamFileId));
delete(TeamFileId) when is_list(TeamFileId) -> 
    delete(list_to_integer(TeamFileId));
delete(TeamFileId) when is_integer(TeamFileId) ->
    case q("delete from teamfiles where id = $1", [TeamFileId]) of
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
    Record = parse_record(Columns, tuple_to_list(DBRecord), #teamfile{}),
    parse_result(Columns, RTail, [Record | Results]).

-spec parse_record(Columns, Values, EmptyRecord) -> Record when
      Columns :: [tuple()],
      Values :: [tuple()],
      EmptyRecord :: #teamfile{},
      Record :: #teamfile{}.
parse_record([], [], Result) -> Result;
parse_record([Column | CTail], [Value | VTail], Result) ->
    {column, Name, _, _, _, _} = Column,
    Result1 = case Name of
                  <<"id">> -> Result#teamfile{id = Value};
                  <<"team_id">> -> Result#teamfile{team_id = Value};
                  <<"name">> -> Result#teamfile{name = Value};
                  <<"owner_id">> -> Result#teamfile{owner_id = Value};
                  <<"description">> -> Result#teamfile{description = Value};
                  <<"created_at">> -> 
                      case Value of
                          undefined -> undefined;
                          V -> 
                              Result#teamfile{
                                created_at = hs_util:pgdatetime_to_seconds(V)}
                      end;
                  <<"updated_at">> -> 
                      case Value of
                          undefined -> undefined;
                          V -> 
                              Result#teamfile{
                                updated_at = hs_util:pgdatetime_to_seconds(V)}
                      end
              end,
    parse_record(CTail, VTail, Result1).

