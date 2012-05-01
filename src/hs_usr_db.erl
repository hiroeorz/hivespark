%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_usr_db).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([q/1, q/2, 
         list/1, insert/6, lookup_id/1, lookup_name/1, update/1, delete/1, 
         authenticate/2,
         add_team/2, delete_team/2, get_team_id_list/1]).

-export([parse_result/3]).

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

%%--------------------------------------------------------------------
%% @doc get usr list from user id list.
%% @end
%%--------------------------------------------------------------------
-spec list(UsrIdList) -> {ok, UsrList} when
      UsrIdList :: [integer() | string() | binary()],
      UsrList :: [] | [#usr{}].
list([]) -> {ok, []};

list([UsrId | _] = UsrIdList) when is_integer(UsrId) ->
    list(lists:map(fun(Id) -> list_to_binary(integer_to_list(Id)) end, 
                   UsrIdList));

list([UsrId | _] = UsrIdList) when is_binary(UsrId) ->
    list(lists:map(fun(Id) -> binary_to_list(Id) end, 
                   UsrIdList));

list([UsrId | _] = UsrIdList) when is_list(UsrId) ->
    Sql = lists:flatten(io_lib:format("select * from usrs where id in (~s)",
                                      [string:join(UsrIdList, ",")])),
    case q(Sql) of
        {ok, Records} -> {ok, Records};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc insert new usr to database.
%% @end
%%--------------------------------------------------------------------
-spec insert(Name, LongName, Mail, Password, IconUrl, Description) -> 
                    {ok, Usr} | {error, Reason} when
      Name :: binary(), 
      LongName :: binary(),
      Mail :: binary(),
      Password :: binary(),
      IconUrl :: binary(),
      Description :: binary(),
      Usr :: #usr{},
      Reason :: atom().
insert(Name, LongName, Mail, Password, IconUrl, Description) ->
    CreatedAt = {date(), time()},
    Seed = create_password_seed(Name, Mail, CreatedAt, Password),
    CryptedPassword = create_crypted_password(Password, Seed),

    Result = q("insert into usrs (name, longname, email, 
                                  password, password_seed, icon_url, 
                                  description, created_at)
                  values($1, $2, $3, $4, $5, $6, $7, $8)
                  returning *",
               [Name, LongName, Mail, CryptedPassword, Seed, IconUrl, 
                Description, CreatedAt]),

    case Result of
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason};
        {ok, [Record]} -> {ok, Record}
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(UsrId) -> {ok, Usr} | {error, not_found} |{error, Reason} when
      UsrId :: integer() | list() | binary(),
      Usr :: #usr{},
      Reason :: tuple().
lookup_id(UsrId) when is_binary(UsrId) -> lookup_id(binary_to_list(UsrId));
lookup_id(UsrId) when is_list(UsrId) -> lookup_id(list_to_integer(UsrId));
lookup_id(UsrId) when is_integer(UsrId) -> 
    case q("select * from usrs where id = $1", [UsrId]) of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.        

%%--------------------------------------------------------------------
%% @doc lookup user by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Usr} | {error, not_found} | {error, Reason} when
      Name :: string(),
      Usr :: #usr{},
      Reason :: tuple().
lookup_name(Name) when is_binary(Name) or is_list(Name) ->
    case q("select * from usrs where name = $1", [Name]) of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.        

%%--------------------------------------------------------------------
%% @doc update user parameter.
%% @end
%%--------------------------------------------------------------------
-spec update(Usr) -> {ok, UpdatedUsr} | {error, Reason} when
      Usr :: #usr{},
      UpdatedUsr :: #usr{},
      Reason :: atom().
update(Usr) ->
    Result = q("update usrs set name = $2,
                                longname = $3,
                                email = $4,
                                icon_url = $5,
                                description = $6,
                                lat = $7,
                                lng = $8
                  where id = $1
                  returning *",
              [Usr#usr.id, Usr#usr.name, Usr#usr.longname, Usr#usr.email,
               Usr#usr.icon_url, Usr#usr.description, Usr#usr.lat, 
               Usr#usr.lng]),

    case Result of
        {ok, [Record]} -> {ok, Record};
        {ok, []} -> {error, empty_result};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc delete usr from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(UsrId) -> {ok, deleted} | {error, Reason} when
      UsrId :: integer() | string() | binary(),
      Reason :: tuple().
delete(UsrId) when is_binary(UsrId) -> delete(binary_to_list(UsrId));
delete(UsrId) when is_list(UsrId) -> delete(list_to_integer(UsrId));
delete(UsrId) when is_integer(UsrId) ->
    case q("delete from usrs where id = $1", [UsrId]) of
        ok -> {ok, deleted};
        {error, Reason} -> {error, Reason}
    end.     

%%--------------------------------------------------------------------
%% @doc user authenticate.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(UsernameBin, PasswordBin) -> {ok, UsrId} | failure when
      UsernameBin :: binary(),
      PasswordBin :: binary(),
      UsrId :: integer().
authenticate(UsernameBin, PasswordBin) ->
    Username = binary_to_list(UsernameBin),

    case lookup_name(Username) of
        {error, _} -> {error, not_found};
        {ok, Usr} ->
            Seed = Usr#usr.password_seed,
            CryptedPassword = create_crypted_password(PasswordBin, Seed),
            SavedPassword = binary_to_list(Usr#usr.password),

            case CryptedPassword of
                SavedPassword -> {ok, Usr#usr.id};
                _ -> failure
            end
    end.

%%--------------------------------------------------------------------
%% @doc add usr's team relation.
%% @end
%%--------------------------------------------------------------------
-spec add_team(UsrId, TeamId) -> ok | {error, already_exist} |  {error, missing_team_id} when
      UsrId :: binary() | integer(),
      TeamId :: binary() | integer().
add_team(UsrId, TeamId) when is_binary(UsrId), is_binary(TeamId) ->
    add_team(list_to_integer(binary_to_list(UsrId)), list_to_integer(binary_to_list(TeamId)));

add_team(UsrId, TeamId) when is_integer(UsrId), is_integer(TeamId) ->
    Result = hs_usr_team_db:q("select * from usrs_teams 
                                        where usr_id = $1 and team_id = $2", 
                                     [UsrId, TeamId]),
    case Result of
        {ok, [_]} -> {error, already_exist};
        {ok, []} ->
            Result1 = hs_team_db:q("select id from teams where id = $1", [TeamId]),

            case Result1 of
                {ok, []} -> {error, missing_team_id};
                {ok, _} ->
                    hs_usr_team_db:q("insert into usrs_teams values ($1, $2, $3)",
                                            [UsrId, TeamId, {date(), time()}])
            end
    end.

%%--------------------------------------------------------------------
%% @doc delete usr's team relation.
%% @end
%%--------------------------------------------------------------------
-spec delete_team(UsrId, TeamId) -> ok when
      UsrId :: binary() | integer(),
      TeamId :: binary() | integer().
delete_team(UsrId, TeamId) ->
    hs_usr_team_db:q("delete from usrs_teams where usr_id = $1 and team_id = $2", 
                            [UsrId, TeamId]).
    


%%--------------------------------------------------------------------
%% @doc get usr's team relation.
%% @end
%%--------------------------------------------------------------------
-spec get_team_id_list(UsrId) -> [#team{}] when
      UsrId :: integer() | binary().
get_team_id_list(UsrId) ->
    {ok, UsrsTeams} = hs_usr_team_db:q("select team_id from usrs_teams 
                                                 where usr_id = $1 order by team_id desc", 
                                              [UsrId]),
    lists:map(fun(UsrTeam) -> UsrTeam#usr_team.team_id end, UsrsTeams).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create crypted password.
%% @end
%%--------------------------------------------------------------------
-spec create_password_seed(Name, Mail, CreatedAt, Password) -> Seed when
      Name :: binary(),
      Mail :: binary(),
      CreatedAt :: tuple(),
      Password :: binary(),
      Seed :: string().
create_password_seed(Name, Mail, CreatedAt, Password) 
  when is_binary(Password)->

    {{Year, Month, Day}, {Hour, Min, Sec}} = CreatedAt,

    TimeStr = integer_to_list(Year) ++ integer_to_list(Month) ++
        integer_to_list(Day) ++ integer_to_list(Hour) ++
        integer_to_list(Min) ++ integer_to_list(Sec),

    SeedBin = crypto:sha([TimeStr, 
                          binary_to_list(Name),
                          binary_to_list(Mail),
                          ?KEY_PHRASE_1,
                          ?KEY_PHRASE_2,
                          ?KEY_PHRASE_3,
                          binary_to_list(Password)]),

    S = lists:flatten(lists:map(fun(X) -> io_lib:format("~.16X", [X, ""]) end, 
                                binary_to_list(SeedBin))),
    string:substr(S, 1, 32).

-spec create_crypted_password(Password, Seed) -> CryptedPassword when
      Password :: binary(),
      Seed :: string(),
      CryptedPassword :: string().
create_crypted_password(Password, Seed) ->
    Bin = crypto:sha([binary_to_list(Password), Seed]),
    P = lists:flatten(lists:map(fun(X) -> io_lib:format("~.16X", [X, ""]) end, 
                                binary_to_list(Bin))),
    string:substr(P, 1, 32).
    

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
    Record = parse_record(Columns, tuple_to_list(DBRecord), #usr{}),
    parse_result(Columns, RTail, [Record | Results]).

-spec parse_record(Columns, Values, EmptyRecord) -> Record when
      Columns :: [tuple()],
      Values :: [tuple()],
      EmptyRecord :: #usr{},
      Record :: #usr{}.
parse_record([], [], Result) -> Result;
parse_record([Column | CTail], [Value | VTail], Result) ->
    {column, Name, _, _, _, _} = Column,
    Result1 = case Name of
                  <<"id">> -> Result#usr{id = Value};
                  <<"name">> -> Result#usr{name = Value};
                  <<"longname">> -> Result#usr{longname = Value};
                  <<"email">> -> Result#usr{email = Value};
                  <<"password">> -> Result#usr{password = Value};
                  <<"password_seed">> -> Result#usr{password_seed = Value};
                  <<"icon_url">> -> Result#usr{icon_url = Value};
                  <<"description">> -> Result#usr{description = Value};
                  <<"lat">> -> Result#usr{lat = Value};
                  <<"lng">> -> Result#usr{lng = Value};
                  <<"created_at">> -> 
                      case Value of
                          undefined -> undefined;
                          V -> 
                              Result#usr{created_at = hs_util:pgdaatetime_to_datetime(V)}
                                  
                      end
              end,

    parse_record(CTail, VTail, Result1).
