%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_usr_cache).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([list/1, store/1, lookup_id/1, lookup_name/1, delete/1, 
         add_team_id_list/2, get_team_id_list/1, delete_team_id_list/1,
         add_worker_pid/2, delete_worker_pid/2, get_worker_pids/1,
         clear_all_worker_pid/0]).

-define(USR_DB, <<"usr_cache">>).
-define(USR_NAME_INDEX_KEY, <<"usr_name_index">>).
-define(USRS_TEAMS_KEY, <<"usrs_teams">>).
-define(USR_PID_KEY, <<"usr_pid_list">>).

-define(KEY_PHRASE_1, "message_box3").
-define(KEY_PHRASE_2, "SHIMANE").
-define(KEY_PHRASE_3, "MATSUE").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc get usr list from user id list.
%% @end
%%--------------------------------------------------------------------
-spec list(UsrIdList) -> UsrList when
      UsrIdList :: [integer() | string() | binary()],
      UsrList :: [#usr{} | undefined].
list([]) -> [];

list([UsrId | _] = UsrIdList) when is_integer(UsrId) ->
    list(lists:map(fun(Id) -> list_to_binary(integer_to_list(Id)) end, 
                   UsrIdList));

list([UsrId | _] = UsrIdList) when is_list(UsrId) ->
    list(lists:map(fun(Id) -> list_to_binary(Id) end, UsrIdList));

list([UsrId | _] = UsrIdList) when is_binary(UsrId) ->
    {ok, List} = eredis_pool:q(?DB_SRV, ["HMGET", ?USR_DB | UsrIdList]),
    Fun = fun(UsrBin, Results) ->
                  case UsrBin of
                      undefined -> [undefined | Results];
                      UsrBin -> [binary_to_term(UsrBin) | Results]
                  end
          end,

    lists:reverse(lists:foldl(Fun, [], List)).

%%--------------------------------------------------------------------
%% @doc store user cache.
%% @end
%%--------------------------------------------------------------------
-spec store(Usr) -> {ok, Usr} | {error, Reason} when
      Usr :: #usr{},
      Reason :: atom().
store(Usr)  ->
    UsrId = Usr#usr.id,
    Name = Usr#usr.name,
    Result = eredis_pool:q(?DB_SRV, 
                           ["HSET", ?USR_DB, UsrId, term_to_binary(Usr)]),
    case  Result of
        {ok, _} ->
            case add_user_name_index(UsrId, Name) of
                ok -> {ok, Usr};
                _ -> {error, index_save_error}
            end;
        Other -> Other
    end.
        

%%--------------------------------------------------------------------
%% @doc lookup user by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(UsrId) -> {ok, Usr} | {error, not_found} when
      UsrId :: integer() | list() | binary(),
      Usr :: #usr{}.
lookup_id(UsrId) when is_integer(UsrId) -> lookup_id(integer_to_list(UsrId));
lookup_id(UsrId) when is_list(UsrId) -> lookup_id(list_to_binary(UsrId));
lookup_id(UsrId) ->
    case eredis_pool:q(?DB_SRV, ["HGET", ?USR_DB, UsrId]) of
        {ok, undefined} -> {error, not_found};
        {ok, Usr} -> {ok, binary_to_term(Usr)}
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Usr} | {error, not_found} when
      Name :: string(),
      Usr :: #usr{}.
lookup_name(Name) when is_list(Name) -> lookup_name(list_to_binary(Name));
lookup_name(Name) when is_binary(Name) ->
    case eredis_pool:q(?DB_SRV, ["HGET", ?USR_NAME_INDEX_KEY, Name]) of
        {ok, undefined} -> {error, not_found};
        {ok, UsrId} -> lookup_id(UsrId)
    end.

%%--------------------------------------------------------------------
%% @doc delete usr from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(UsrId) -> {ok, deleted} | {error, not_found} when
      UsrId :: integer() | string() | binary().
delete(UsrId) when is_integer(UsrId) -> delete(integer_to_list(UsrId));
delete(UsrId) when is_list(UsrId) -> delete(list_to_binary(UsrId));
delete(UsrId) when is_binary(UsrId) ->
    case lookup_id(UsrId) of
        {error, not_found} -> {error, not_found};
        {ok, Usr} ->
            case eredis_pool:q(?DB_SRV, ["HDEL", ?USR_DB, UsrId]) of
                {ok, <<"0">>} -> {error, not_found};
                {ok, <<"1">>} ->
                    eredis_pool:q(?DB_SRV, 
                                  ["HDEL", ?USR_NAME_INDEX_KEY, Usr#usr.name]),
                    {ok, deleted}
            end
    end.
    
%%--------------------------------------------------------------------
%% @doc set team id list to redis.
%% @end
%%--------------------------------------------------------------------
-spec add_team_id_list(UsrId, TeamIds) -> ok when
      UsrId :: integer() | binary(),
      TeamIds :: [integer()].
add_team_id_list(UsrId, TeamIds) when is_integer(UsrId) -> 
    add_team_id_list(list_to_binary(integer_to_list(UsrId)), TeamIds);

add_team_id_list(UsrId, TeamIds) when is_binary(UsrId) ->
    case eredis_pool:q(?DB_SRV, ["HSET", ?USRS_TEAMS_KEY, UsrId, term_to_binary(TeamIds)]) of
        {ok, _} -> ok;
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc get team id list from redis.
%% @end
%%--------------------------------------------------------------------
-spec get_team_id_list(UserId) -> TeamIds  when
      UserId :: integer() | binary(),
      TeamIds :: [#team{}].
get_team_id_list(UsrId) when is_integer(UsrId) ->          
    get_team_id_list(list_to_binary(integer_to_list(UsrId)));

get_team_id_list(UsrId) when is_binary(UsrId) -> 
    case eredis_pool:q(?DB_SRV, ["HGET", ?USRS_TEAMS_KEY, UsrId]) of
        {ok, undefined} -> [];
        {ok, TeamIds} -> binary_to_term(TeamIds)
    end.

%%--------------------------------------------------------------------
%% @doc delete team id list from redis.
%% @end
%%--------------------------------------------------------------------
-spec delete_team_id_list(UsrId) -> ok when
      UsrId :: integer() | binary().
delete_team_id_list(UsrId) ->
    {ok, _} = eredis_pool:q(?DB_SRV, ["HDEL", ?USRS_TEAMS_KEY, UsrId]),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% add usr's websocket worker pid.
%% @end
%%--------------------------------------------------------------------
-spec add_worker_pid(UsrId, Pid) -> ok when
      UsrId :: integer(),
      Pid :: pid().
add_worker_pid(UsrId, Pid) when is_integer(UsrId) and is_pid(Pid) ->
    PidList = get_worker_pids(UsrId),

    case lists:any(fun(A) -> A == Pid end, PidList) of
        true -> ok;
        false ->
            PidListBin = term_to_binary([Pid | PidList]),
            {ok, _} = eredis_pool:q(?DB_SRV, ["HSET", ?USR_PID_KEY, 
                                              UsrId, PidListBin])
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc 
%% delete usr's websocket worker pid.
%% @end
%%--------------------------------------------------------------------
-spec delete_worker_pid(UsrId, Pid) -> ok when
      UsrId :: integer(),
      Pid :: pid().
delete_worker_pid(UsrId, Pid) when is_integer(UsrId) and is_pid(Pid) ->
    IdList = get_worker_pids(UsrId),
    NewIdListBin = term_to_binary(lists:delete(Pid, IdList)),

    {ok, _} = eredis_pool:q(?DB_SRV, ["HSET", ?USR_PID_KEY, 
                                      UsrId, NewIdListBin]),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% clear all websocket worker pid.
%% @end
%%--------------------------------------------------------------------
-spec clear_all_worker_pid() -> ok.
clear_all_worker_pid() ->
    {ok, _} = eredis_pool:q(?DB_SRV, ["DEL", ?USR_PID_KEY]),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% get usr's websocket workers.
%% @end
%%--------------------------------------------------------------------
-spec get_worker_pids(UsrId) -> [pid()] | [] when
      UsrId :: integer().
get_worker_pids(UsrId) ->
    case eredis_pool:q(?DB_SRV, ["HGET", ?USR_PID_KEY, 
                                 list_to_binary(integer_to_list(UsrId))]) of
        {ok, undefined} -> [];
        {ok, PidListBin} -> binary_to_term(PidListBin)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_user_name_index(UsrId, Name) -> ok | Error when
      UsrId :: integer(),
      Name :: string(),
      Error :: tuple().
add_user_name_index(UsrId, Name) ->
    case eredis_pool:q(?DB_SRV, ["HSET", ?USR_NAME_INDEX_KEY, Name, UsrId]) of
        {ok, _} -> ok;
        Error -> Error
    end.
