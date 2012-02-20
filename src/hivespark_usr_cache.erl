%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hivespark_usr_cache).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([list/1, store/1, lookup_id/1, lookup_name/1, delete/1]).

-define(MaxIdKey, <<"max_usr_id">>).
-define(USR_DB, <<"usr_cache">>).
-define(USR_NAME_INDEX_KEY, <<"usr_name_index">>).

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
