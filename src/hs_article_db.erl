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
-export([q/1, q/2, insert/1, list_of_team/3, list_of_usr/2, to_tuple/1]).

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
    Result =  q("insert into articles (usr_id, team_id, title, text, created_at, 
                                       lat, lng)
                   values ($1, $2, $3, $4, $5, $6, $7) returning *",
                [Article#article.usr_id, Article#article.team_id,
                 Article#article.title, Article#article.text, 
                 {date(), time()},
                 Article#article.lat, Article#article.lng]),

    case Result of
        {ok, [NewArticle]} -> {ok, NewArticle};
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason}
    end.

-spec list_of_team(TeamId, Offset, Count) -> {ok, Articles} | {error, Reason} when
      TeamId :: integer() | binary(),
      Offset :: integer() | binary(),
      Count :: integer() | binary(),
      Articles :: [#article{}],
      Reason :: atom().      
list_of_team(TeamId, Offset, Count) when is_binary(TeamId) and
                                         is_binary(Offset) and
                                         is_binary(Count) ->
    list_of_team(list_to_integer(binary_to_list(TeamId)),
                 list_to_integer(binary_to_list(Offset)),
                 list_to_integer(binary_to_list(Count)));

list_of_team(TeamId, Offset, Count) when is_integer(TeamId) and
                                 is_integer(Offset) and
                                 is_integer(Count)->
    q("select * from articles where team_id = $1
         order by id desc limit $2 offset $3", 
      [TeamId, Count, Offset]).
    
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
      {text, Article#article.text}, {created_at, DateTime},
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
                  <<"created_at">> -> Result#article{created_at = Value};
                  <<"lat">> -> Result#article{lat = Value};
                  <<"lng">> -> Result#article{lng = Value}
              end,
    parse_record(CTail, VTail, Result1).
