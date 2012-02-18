%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hivespark_usr_db).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([list/1, insert/6, lookup_id/1, lookup_name/1, update/7, delete/1]).

-define(MaxIdKey, <<"max_usr_id">>).
-define(USR_DB, <<"usr_db">>).
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
    case lookup_name(Name) of
        {ok, _} -> {error, already_exist};
        {error, not_found} ->
            UsrId = get_next_id(),
            Usr1 = #usr{id = UsrId, name = Name, longname = LongName,
                        password = Password, email = Mail,
                        description = Description,
                        icon_url = IconUrl,
                        created_at = {date(), time()}},
            
            CryptedPassword = create_crypted_password(Usr1, Password),
            Usr2 = Usr1#usr{password = CryptedPassword},
            
            case eredis_pool:q(?DB_SRV, 
                               ["HSET", ?USR_DB, UsrId, term_to_binary(Usr2)]) of
                {ok, _} ->
                    case add_user_name_index(UsrId, Name) of
                        ok -> {ok, Usr2};
                        _ -> {error, index_save_error} %% todo 後始末
                    end;
                Other -> Other
            end
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
%% @doc update user parameter.
%% @end
%%--------------------------------------------------------------------
-spec update(UsrId, Name, LongName, Mail, Password, IconUrl, Description) -> 
                    {ok, Usr} | {error, Reason} when
      UsrId :: integer(),
      Name :: binary(), 
      LongName :: binary(),
      Mail :: binary(),
      Password :: binary(),
      IconUrl :: binary(),
      Description :: binary(),
      Usr :: #usr{},
      Reason :: atom().
update(UsrId, Name, LongName, Mail, Password, IconUrl, Description) ->
    case lookup_id(UsrId) of
        {error, not_found} -> {error, not_found};
        {ok, Usr} ->
            Usr1 = Usr#usr{name = Name,
                           longname = LongName,
                           email = Mail,
                           icon_url = IconUrl,
                           description = Description},
            Usr2 = case Password of
                       <<"">> -> 
                           Usr1;
                       Password1 ->
                           Crypted = create_crypted_password(Usr1, Password1),
                           Usr1#usr{password = Crypted}
                   end,

            Result = eredis_pool:q(?DB_SRV, ["HSET", ?USR_DB, 
                                             UsrId, term_to_binary(Usr2)]),
            
            case  Result of
                {ok, _} -> {ok, Usr2};
                Other -> Other
            end
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

-spec get_next_id() -> NextId when
      NextId ::integer().
get_next_id() ->
    {ok, NextIdBin} = eredis_pool:q(?DB_SRV, ["INCR", ?MaxIdKey]),
    list_to_integer(binary_to_list(NextIdBin)).

-spec add_user_name_index(UsrId, Name) -> ok | Error when
      UsrId :: integer(),
      Name :: string(),
      Error :: tuple().
add_user_name_index(UsrId, Name) ->
    case eredis_pool:q(?DB_SRV, ["HSET", ?USR_NAME_INDEX_KEY, Name, UsrId]) of
        {ok, _} -> ok;
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc create crypted password.
%% @end
%%--------------------------------------------------------------------
-spec create_crypted_password(Usr, Password) -> CryptedPassword when
      Usr::#usr{},
      Password::binary(),
      CryptedPassword::string().

create_crypted_password(Usr, Password) when is_binary(Password)->

    {{Year, Month, Day}, {Hour, Min, Sec}} = Usr#usr.created_at,

    TimeStr = integer_to_list(Year) ++ integer_to_list(Month) ++
        integer_to_list(Day) ++ integer_to_list(Hour) ++
        integer_to_list(Min) ++ integer_to_list(Sec),

    CryptedPassword = crypto:sha([TimeStr, 
                                  binary_to_list(Usr#usr.name),
                                  binary_to_list(Usr#usr.email),
                                  ?KEY_PHRASE_1,
                                  ?KEY_PHRASE_2,
                                  ?KEY_PHRASE_3,
                                  binary_to_list(Password)]),

    lists:flatten(lists:map(fun(X) -> 
                                    io_lib:format("~.16X", [X, ""]) 
                            end, 
                            binary_to_list(CryptedPassword))).
