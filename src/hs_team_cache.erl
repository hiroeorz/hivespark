%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_team_cache).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([list/1, store/1, lookup_id/1, lookup_name/1, delete/1]).

-define(MaxIdKey, <<"max_usr_id">>).
-define(TEAM_DB, <<"team_cache">>).
-define(TEAM_NAME_INDEX_KEY, <<"team_name_index">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc get team list from user id list.
%% @end
%%--------------------------------------------------------------------
-spec list(TeamIdList) -> TeamList when
      TeamIdList :: [integer() | string() | binary()],
      TeamList :: [#team{} | undefined].
list([]) -> [];

list([TeamId | _] = TeamIdList) when is_integer(TeamId) ->
    list(lists:map(fun(Id) -> list_to_binary(integer_to_list(Id)) end, 
                   TeamIdList));

list([TeamId | _] = TeamIdList) when is_list(TeamId) ->
    list(lists:map(fun(Id) -> list_to_binary(Id) end, TeamIdList));

list([TeamId | _] = TeamIdList) when is_binary(TeamId) ->
    {ok, List} = eredis_pool:q(?DB_SRV, ["HMGET", ?TEAM_DB | TeamIdList]),
    Fun = fun(TeamBin, Results) ->
                  case TeamBin of
                      undefined -> [undefined | Results];
                      TeamBin -> [binary_to_term(TeamBin) | Results]
                  end
          end,

    lists:reverse(lists:foldl(Fun, [], List)).

%%--------------------------------------------------------------------
%% @doc store team cache.
%% @end
%%--------------------------------------------------------------------
-spec store(Team) -> {ok, Team} | {error, Reason} when
      Team :: #team{},
      Reason :: atom().
store(Team) ->
    TeamId = Team#team.id,
    Name = Team#team.name,
    Result = eredis_pool:q(?DB_SRV, 
                           ["HSET", ?TEAM_DB, TeamId, term_to_binary(Team)]),
    case  Result of
        {ok, _} ->
            case add_team_name_index(TeamId, Name) of
                ok -> {ok, Team};
                _ -> {error, index_save_error}
            end;
        Other -> Other
    end.

%%--------------------------------------------------------------------
%% @doc lookup team by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(TeamId) -> {ok, Team} | {error, not_found} when
      TeamId :: integer() | list() | binary(),
      Team :: #team{}.
lookup_id(TeamId) when is_integer(TeamId) -> lookup_id(integer_to_list(TeamId));
lookup_id(TeamId) when is_list(TeamId) -> lookup_id(list_to_binary(TeamId));
lookup_id(TeamId) ->
    case eredis_pool:q(?DB_SRV, ["HGET", ?TEAM_DB, TeamId]) of
        {ok, undefined} -> {error, not_found};
        {ok, Team} -> {ok, binary_to_term(Team)}
    end.

%%--------------------------------------------------------------------
%% @doc lookup team by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Team} | {error, not_found} when
      Name :: string(),
      Team :: #team{}.
lookup_name(Name) when is_list(Name) -> lookup_name(list_to_binary(Name));
lookup_name(Name) when is_binary(Name) ->
    case eredis_pool:q(?DB_SRV, ["HGET", ?TEAM_NAME_INDEX_KEY, Name]) of
        {ok, undefined} -> {error, not_found};
        {ok, TeamId} -> lookup_id(TeamId)
    end.

%%--------------------------------------------------------------------
%% @doc delete team from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(TeamId) -> {ok, deleted} | {error, not_found} when
      TeamId :: integer() | string() | binary().
delete(TeamId) when is_integer(TeamId) -> delete(integer_to_list(TeamId));
delete(TeamId) when is_list(TeamId) -> delete(list_to_binary(TeamId));
delete(TeamId) when is_binary(TeamId) ->
    case lookup_id(TeamId) of
        {error, not_found} -> {error, not_found};
        {ok, Team} ->
            case eredis_pool:q(?DB_SRV, ["HDEL", ?TEAM_DB, TeamId]) of
                {ok, <<"0">>} -> {error, not_found};
                {ok, <<"1">>} ->
                    eredis_pool:q(?DB_SRV, 
                                  ["HDEL", ?TEAM_NAME_INDEX_KEY, 
                                   Team#team.name]),
                    {ok, deleted}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_team_name_index(TeamId, Name) -> ok | Error when
      TeamId :: integer(),
      Name :: string(),
      Error :: tuple().
add_team_name_index(TeamId, Name) ->
    case eredis_pool:q(?DB_SRV, ["HSET", ?TEAM_NAME_INDEX_KEY, Name, TeamId]) of
        {ok, _} -> ok;
        Error -> Error
    end.
