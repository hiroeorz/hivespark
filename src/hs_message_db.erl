%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_message_db).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([q/1, q/2, insert/1, get_msg/1, list_of_team/3, 
         list_of_team_by_since_id/2, list_of_usr/2,
         get_latest_of_team/1]).

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

-spec insert(Message) -> {ok, Message} | {error, Reason} when
      Message :: #message{},
      Reason :: atom().
insert(Message) ->
    Result =  q("insert into messages (usr_id, team_id, text, created_at, 
                                       lat, lng, 
                                       in_article_id,
                                       in_reply_to_id)
                   values ($1, $2, $3, $4, $5, $6, $7, $8) returning *",
                [Message#message.usr_id, 
                 Message#message.team_id, 
                 Message#message.text, 
                 {date(), time()},
                 Message#message.lat, 
                 Message#message.lng,
                 Message#message.in_article_id,
                 Message#message.in_reply_to_id]),

    case Result of
        {ok, [NewMessage]} -> {ok, NewMessage};
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason}
    end.

-spec get_msg(Id) -> {ok, Message} | {error, Reason} when
      Id :: integer() | binary(),
      Message :: #message{},
      Reason :: atom().
get_msg(MsgId) when is_binary(MsgId) ->
    get_msg(list_to_integer(binary_to_list(MsgId)));

get_msg(MsgId) when is_integer(MsgId) ->
    case q("select * from messages where id = $1", [MsgId]) of
        {ok, [Msg]} -> {ok, Msg};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec list_of_team(TeamId, Offset, Count) -> {ok, Messages} | {error, Reason} when
      TeamId :: integer() | binary(),
      Offset :: integer() | binary(),
      Count :: integer() | binary(),
      Messages :: [#message{}],
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
    q("select * from messages where team_id = $1
         order by id desc limit $2 offset $3", 
      [TeamId, Count, Offset]).

-spec list_of_team_by_since_id(TeamId, SinceId) -> {ok, Messages} | 
                                                   {error, Reason} when
      TeamId :: integer() | binary(),
      SinceId :: integer() | binary(),
      Messages :: [#message{}],
      Reason :: atom().      
list_of_team_by_since_id(TeamId, SinceId) when is_binary(TeamId) and
                                               is_binary(SinceId) ->
    list_of_team_by_since_id(list_to_integer(binary_to_list(TeamId)), 
                             list_to_integer(binary_to_list(SinceId)));

list_of_team_by_since_id(TeamId, SinceId) when is_integer(TeamId) and
                                               is_integer(SinceId) ->

    q("select * from messages 
         where team_id = $1 and id > $2
         order by id desc", 
      [TeamId, SinceId]).
    
-spec list_of_usr(UsrId, Count) -> {ok, Messages} | {error, Reason} when
      UsrId :: integer() | binary(),
      Count :: integer() | binary(),
      Messages :: [#message{}],
      Reason :: atom().      
list_of_usr(UsrId, Count) when is_binary(UsrId) and
                               is_binary(Count) ->
    list_of_usr(list_to_integer(binary_to_list(UsrId)),
                list_to_integer(binary_to_list(Count)));

list_of_usr(UsrId, Count) when is_integer(UsrId) and
                        is_integer(Count) ->
    q("select * from messages where usr_id = $1
         order by id desc
         limit $2", [UsrId, Count]).

%%--------------------------------------------------------------------
%% @doc
%% チームに投稿された最後のメッセージを返す。
%% @end
%%--------------------------------------------------------------------
-spec get_latest_of_team(TeamId) -> Message when
      TeamId :: integer() | binary(),
      Message :: #message{} | undefined.
get_latest_of_team(TeamId) when is_binary(TeamId) ->
    get_latest_of_team(list_to_integer(binary_to_list(TeamId)));

get_latest_of_team(TeamId) when is_integer(TeamId) ->
    Result =  q("select * from messages where team_id = $1
                   order by id desc
                   limit 1", 
                [TeamId]),
    case Result of
        {ok, []} -> undefined;
        {ok, [Message]} -> Message
    end.
            


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
    Record = parse_record(Columns, tuple_to_list(DBRecord), #message{}),
    parse_result(Columns, RTail, [Record | Results]).

-spec parse_record(Columns, Values, EmptyRecord) -> Record when
      Columns :: [tuple()],
      Values :: [tuple()],
      EmptyRecord :: #message{},
      Record :: #message{}.
parse_record([], [], Result) -> Result;
parse_record([Column | CTail], [Val | VTail], Result) ->
    {column, Name, _, _, _, _} = Column,
    Result1 = case Name of
                  <<"id">> -> Result#message{id = Val};
                  <<"usr_id">> -> Result#message{usr_id = Val};
                  <<"team_id">> -> Result#message{team_id = Val};
                  <<"text">> -> Result#message{text = Val};
                  <<"in_article_id">> -> Result#message{in_article_id = Val};
                  <<"in_reply_to_id">> -> Result#message{in_reply_to_id = Val};
                  <<"lat">> -> Result#message{lat = Val};
                  <<"lng">> -> Result#message{lng = Val};
                  <<"created_at">> -> 
                      case Val of
                          undefined -> undefined;
                          V -> 
                              Result#message{
                                created_at = hs_util:pgdatetime_to_seconds(V)}
                      end
              end,
    parse_record(CTail, VTail, Result1).
