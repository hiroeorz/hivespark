%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_teamfile).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/5, get_file/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 受け取ったデータをAmazonS3へ保存して付随情報をローカルのデータベースへ保存する
%% @end
%%--------------------------------------------------------------------
-spec create(TeamId, OwnerId, Name, Description, Contents) ->  
                    {ok, TeamFile} | {error, Reason} when
      TeamId :: integer(),
      OwnerId :: integer(),
      Name :: binary(),
      Description :: binary(),
      Contents :: binary(),
      TeamFile :: #teamfile{},
      Reason :: atom().

create(TeamId, OwnerId, Name, Description, Contents) when is_integer(TeamId) and
                                                          is_integer(OwnerId) and
                                                          is_binary(Name) and
                                                          is_binary(Description) and
                                                          is_binary(Contents) ->
    case hs_teamfile_db:insert(TeamId, OwnerId, Name, Description) of
        {error, Reason} -> {error, Reason};
        {ok, TeamFile} ->
            Key = get_key(TeamFile#teamfile.id),
            
            case hs_file:save(Key, Contents) of
                {s3_error, Reason} -> 
                    {ok, deleted} = hs_teamfile:delete(TeamFile#teamfile.id),
                    {s3_error, Reason};
                {ok, _} ->
                    {ok, TeamFile#teamfile{contents = Contents}}
            end
    end.

-spec get_file(TeamFileId) -> {ok, TeamFile} | {aws_error, ErrMsg} when
      TeamFileId :: integer() | binary(),
      TeamFile :: #teamfile{},
      ErrMsg :: tuple().
get_file(TeamFileId) when is_binary(TeamFileId) ->
    get_file(list_to_integer(binary_to_list(TeamFileId)));

get_file(TeamFileId) when is_integer(TeamFileId) ->
    case hs_teamfile_db:lookup_id(TeamFileId) of
        {error, Reason} -> {error, Reason};
        {ok, TeamFile} ->
            Key = get_key(TeamFile#teamfile.id),

            case hs_file:read(Key) of
                {aws_error, ErrMsg} -> {aws_error, ErrMsg};
                {ok, Contents, _Res} ->
                    {ok, TeamFile#teamfile{contents = Contents}}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Amazon S3 へ保存する為のキーを作る
-spec get_key(TeamFileId) -> Key when
      TeamFileId :: integer(),
      Key :: string().
get_key(TeamFileId) when is_integer(TeamFileId) ->
    "hs_teamfile_" ++ integer_to_list(TeamFileId).
