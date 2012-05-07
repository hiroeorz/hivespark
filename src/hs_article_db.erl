%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_article_db).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([q/1, q/2, insert/1, lookup_id/1, update/1, 
         list_of_team/4, list_of_usr/2, 
         to_tuple/1]).

-export([parse_result/3]).

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

-spec insert(Article) -> {ok, Article} | {error, Reason} when
      Article :: #article{},
      Reason :: atom().
insert(Article) ->
    Result =  q("insert into articles (usr_id, team_id, title, text, type, 
                                       created_at, lat, lng, status)
                   values ($1, $2, $3, $4, $5, $6, $7, $8, $9) returning *",
                [Article#article.usr_id, Article#article.team_id,
                 Article#article.title, Article#article.text, 
                 Article#article.type, {date(), time()},
                 Article#article.lat, Article#article.lng,
                 Article#article.status]),

    case Result of
        {ok, [NewArticle]} -> {ok, NewArticle};
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason}
    end.

-spec lookup_id(ArticleId) -> {ok, Article} | {error, not_found} when
      ArticleId :: binary() | integer(),
      Article :: #article{}.
lookup_id(ArticleId) when is_binary(ArticleId) ->
    lookup_id(list_to_integer(binary_to_list(ArticleId)));

lookup_id(ArticleId) when is_integer(ArticleId)->
    case q("select * from articles where id = $1 limit 1", [ArticleId]) of
        {ok, [Article]} -> {ok, Article};
        {ok, []} -> {error, not_found}
    end.

-spec update(Article) -> {ok, UpdatedArticle} | {error, Reason} when
      Article :: #article{},
      UpdatedArticle :: #article{},
      Reason :: atom().
update(Article) ->
    Result = q("update articles set usr_id = $2, team_id = $3,
                                    title = $4, text = $5, type = $6,
                                    lat = $7, lng = $8, status = $9,
                                    progress = $10
                  where id = $1
                  returning *",
              [Article#article.id, 
               Article#article.usr_id, Article#article.team_id,
               Article#article.title, Article#article.text, 
               Article#article.type, 
               Article#article.lat, Article#article.lng,
               Article#article.status, Article#article.progress]),

    case Result of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec list_of_team(TeamId, Offset, Count, Status) -> {ok, Articles} | {error, Reason} when
      TeamId :: integer() | binary(),
      Offset :: integer() | binary(),
      Count :: integer() | binary(),
      Status :: integer() | binary(),
      Articles :: [#article{}],
      Reason :: atom().      
list_of_team(TeamId, Offset, Count, Status) when is_binary(TeamId) and
                                                 is_binary(Offset) and
                                                 is_binary(Count) ->
    list_of_team(list_to_integer(binary_to_list(TeamId)),
                 list_to_integer(binary_to_list(Offset)),
                 list_to_integer(binary_to_list(Count)),
                 list_to_integer(binary_to_list(Status)));

list_of_team(TeamId, Offset, Count, Status) when is_integer(TeamId) and
                                                 is_integer(Offset) and
                                                 is_integer(Count) and
                                                 is_integer(Status) ->
    q("select * from articles where team_id = $1 and status = $2
         order by id desc limit $3 offset $4", 
      [TeamId, Status, Count, Offset]).
    
-spec list_of_usr(UsrId, Count) -> {ok, Articles} | {error, Reason} when
      UsrId :: integer() | binary(),
      Count :: integer() | binary(),
      Articles :: [#article{}],
      Reason :: atom().      
list_of_usr(UsrId, Count) when is_binary(UsrId) and
                               is_binary(Count) ->
    list_of_usr(list_to_integer(binary_to_list(UsrId)),
                list_to_integer(binary_to_list(Count)));

list_of_usr(UsrId, Count) when is_integer(UsrId) and
                        is_integer(Count) ->
    q("select * from articles where usr_id = $1
         order by id desc
         limit $2", [UsrId, Count]).

%%--------------------------------------------------------------------
%% @doc parse usr for json object.
%% @end
%%--------------------------------------------------------------------
-spec to_tuple(Article) -> TupleArticle when
      Article :: #article{},
      TupleArticle :: tuple().
to_tuple(Article) ->
    {ok, Usr} = hs_usr:lookup_id(Article#article.usr_id),
    {ok, Team} = hs_team:lookup_id(Article#article.team_id),
    DateTime = hs_util:create_datetime_string(Article#article.created_at),

    {[{id, Article#article.id}, {usr, hs_usr:to_tuple(Usr)},
      {team, hs_team:to_tuple(Team)}, {title, Article#article.title},
      {text, Article#article.text}, {type, Article#article.type}, 
      {status, Article#article.status}, {progress, Article#article.progress},
      {created_at, DateTime},
      {lat, Article#article.lat}, {lng, Article#article.lng}]}.

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
    Record = parse_record(Columns, tuple_to_list(DBRecord), #article{}),
    parse_result(Columns, RTail, [Record | Results]).

-spec parse_record(Columns, Values, EmptyRecord) -> Record when
      Columns :: [tuple()],
      Values :: [tuple()],
      EmptyRecord :: #article{},
      Record :: #article{}.
parse_record([], [], Result) -> Result;
parse_record([Column | CTail], [Value | VTail], Result) ->
    {column, Name, _, _, _, _} = Column,
    Result1 = case Name of
                  <<"id">> -> Result#article{id = Value};
                  <<"usr_id">> -> Result#article{usr_id = Value};
                  <<"team_id">> -> Result#article{team_id = Value};
                  <<"title">> -> Result#article{title = Value};
                  <<"text">> -> Result#article{text = Value};
                  <<"type">> -> Result#article{type = Value};
                  <<"status">> -> Result#article{status = Value};
                  <<"progress">> -> Result#article{progress = Value};
                  <<"lat">> -> Result#article{lat = Value};
                  <<"lng">> -> Result#article{lng = Value};
                  <<"created_at">> -> 
                      case Value of
                          undefined -> undefined;
                          V -> 
                              Result#article{
                                created_at = hs_util:pgdaatetime_to_seconds(V)}
                      end 
              end,
    parse_record(CTail, VTail, Result1).
