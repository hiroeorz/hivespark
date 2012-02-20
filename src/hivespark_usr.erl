%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hivespark_usr).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/6, delete/1, lookup_id/1, lookup_name/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create new user.
%% @end
%%--------------------------------------------------------------------
-spec create(Name, LongName, Mail, Password, IconUrl, Description) -> 
                    {ok, Usr} | {error, Reason} when
      Name :: binary(),
      LongName :: binary(),
      Mail :: binary(),
      Password :: binary(),
      IconUrl :: binary(),
      Description :: binary(),
      Usr :: #usr{},
      Reason :: atom().
create(Name, LongName, Mail, Password, IconUrl, Description) ->
    case hivespark_usr_db:lookup_name(Name) of
        {ok, _Usr} ->
            {error, already_exist};
        {error, not_found} ->
            hivespark_usr_db:insert(Name, LongName, Mail, Password, 
                                    IconUrl, Description)
    end.

%%--------------------------------------------------------------------
%% @doc delete user.
%% @end
%%--------------------------------------------------------------------
-spec delete(UsrId) -> {ok, deleted} | {error, not_found} when
      UsrId :: integer() | string() | binary().
delete(UsrId) ->
    case hivespark_usr_db:lookup_id(UsrId) of
        {error, not_found} ->
            {error, not_found};
        {ok, _Usr} ->
            hivespark_usr_cache:delete(UsrId),
            {ok, deleted} = hivespark_usr_db:delete(UsrId),
            {ok, deleted}
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(UsrId) -> {ok, Usr} | {error, not_found} when
      UsrId :: integer() | list() | binary(),
      Usr :: #usr{}.
lookup_id(UsrId) ->
    case hivespark_usr_cache:lookup_id(UsrId) of
        {ok, Usr} -> {ok, Usr};
        {error, not_found} ->
            case hivespark_usr_db:lookup_id(UsrId) of
                {error, not_found} -> {error, not_found};
                {ok, Usr} -> 
                    hivespark_usr_cache:store(Usr),
                    {ok, Usr}
            end
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Usr} | {error, not_found} when
      Name :: string(),
      Usr :: #usr{}.
lookup_name(Name) ->
    case hivespark_usr_cache:lookup_name(Name) of
        {ok, Usr} -> {ok, Usr};
        {error, not_found} ->
            case hivespark_usr_db:lookup_name(Name) of
                {error, not_found} -> {error, not_found};
                {ok, Usr} -> 
                    hivespark_usr_cache:store(Usr),
                    {ok, Usr}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
