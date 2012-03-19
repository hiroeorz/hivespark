%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_usr_team_db).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([q/1, q/2, get_teams_usrs/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc exec sql query.
%% @end
%%--------------------------------------------------------------------
-spec q(Sql) -> ok | {ok, list()} | {error, Reason} when
      Sql :: string(),
      Reason :: tuple().
q(Sql) -> q(Sql, []).

-spec q(Sql, Params) -> ok | {ok, [#usr_team{}]} | {error, Reason} when
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
%% @doc get usrs from team_id.
%% @end
%%--------------------------------------------------------------------
-spec get_teams_usrs(TeamId) -> {ok, [#usr{}]} | {error, Reason} when
      TeamId :: integer(),
      Reason :: atom().
get_teams_usrs(TeamId) ->
    Result =  postgres_pool:equery(?DB, "select u.* from usrs u, usrs_teams ut
                                           where team_id = $1 and
                                                 ut.usr_id = u.id
                                           order by usr_id", 
                                   [TeamId]),
    case Result of
        {error, Reason} -> {error, Reason};
        {ok, Columns, Values} -> 
            {ok, hs_usr_db:parse_result(Columns, Values, [])}
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
    Record = parse_record(Columns, tuple_to_list(DBRecord), #usr_team{}),
    parse_result(Columns, RTail, [Record | Results]).

-spec parse_record(Columns, Values, EmptyRecord) -> Record when
      Columns :: [tuple()],
      Values :: [tuple()],
      EmptyRecord :: #usr_team{},
      Record :: #usr_team{}.
parse_record([], [], Result) -> Result;
parse_record([Column | CTail], [Value | VTail], Result) ->
    {column, Name, _, _, _, _} = Column,
    Result1 = case Name of
                  <<"usr_id">> -> Result#usr_team{usr_id = Value};
                  <<"team_id">> -> Result#usr_team{team_id = Value};
                  <<"created_at">> -> Result#usr_team{created_at = Value}
              end,
    parse_record(CTail, VTail, Result1).
