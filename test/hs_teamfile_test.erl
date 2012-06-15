-module(hs_teamfile_test).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, [pgdatetime_to_seconds/1]).
-import(hs_mock, [pg_teamfile_fields_tuples/0,
                  pg_record_teamfile_1/0, record_teamfile_1/0,
                  pg_record_teamfile_2/0, record_teamfile_2/0,
                  pg_record_teamfile_3/0, record_teamfile_3/0]).

create_test_() ->
    {spawn,
     {setup, 
      fun()  -> setup(), start_server() end, 
      fun(_) -> teardown() end,
      [
       {"データの保存に成功",
        fun() ->
                Data = <<"this is data 1">>,
                TeamFile1 = record_teamfile_1(),
                TeamFile2 = TeamFile1#teamfile{contents = Data},

                TeamId = TeamFile2#teamfile.team_id, 
                OwnerId = TeamFile2#teamfile.owner_id, 
                Name = list_to_binary(TeamFile2#teamfile.name), 
                Description = list_to_binary(TeamFile2#teamfile.description),

                meck:expect(postgres_pool, equery, 
                            fun(_, _, P) -> 
                                    ?assertMatch([TeamId, 
                                                  Name, 
                                                  OwnerId, 
                                                  Description, 
                                                  {{_,_,_}, {_,_,_}}, 
                                                  {{_,_,_}, {_,_,_}}], P),

                                    {ok, pg_teamfile_fields_tuples(), 
                                     [pg_record_teamfile_1()]}
                            end),

                meck:expect(erlcloud_s3, put_object,
                           fun(_Bucket, Akey, Aval, _S3) ->
                                   ?assertEqual("hs_teamfile_1", Akey),
                                   ?assertEqual(Data, Aval),
                                   [{version_id, "null"}]
                           end),

                ?assertEqual({ok, TeamFile2}, hs_teamfile:create(TeamId, 
                                                                 OwnerId, 
                                                                 Name, 
                                                                 Description,
                                                                 Data)),

                ?assert(meck:validate(postgres_pool)),
                ?assert(meck:validate(erlcloud_s3))
        end
       }
      ]
     }
    }.

get_file_test_() ->
    {spawn,
     {setup, 
      fun()  -> setup(), start_server() end, 
      fun(_) -> teardown() end,
      [
       {"データの取得に成功",
        fun() ->
                Data = <<"this is file 1">>,
                S3_Obj = s3_object(Data),
                TeamFile1 = record_teamfile_1(),
                TeamFile2 = TeamFile1#teamfile{contents = Data},
                TeamFileId = TeamFile2#teamfile.id,

                meck:expect(postgres_pool, equery, 
                            fun(_, _, P) -> 
                                    ?assertEqual([TeamFileId], P),
                                    {ok, pg_teamfile_fields_tuples(), 
                                     [pg_record_teamfile_1()]}
                            end),

                meck:expect(erlcloud_s3, get_object,
                           fun(_Bucket, Akey, _S3) ->
                                   ?assertEqual("hs_teamfile_1", Akey),
                                   S3_Obj
                           end),


                ?assertEqual({ok, TeamFile2}, 
                             hs_teamfile:get_file(TeamFileId)),

                ?assert(meck:validate(postgres_pool)),
                ?assert(meck:validate(erlcloud_s3))
        end
       },

       {"AmazonS3上にデータが存在しない",
        fun() ->
                Data = <<"this is file 1">>,
                TeamFile1 = record_teamfile_1(),
                TeamFile2 = TeamFile1#teamfile{contents = Data},
                TeamFileId = TeamFile2#teamfile.id,

                meck:expect(postgres_pool, equery, 
                            fun(_, _, P) -> 
                                    ?assertEqual([TeamFileId], P),
                                    {ok, pg_teamfile_fields_tuples(), 
                                     [pg_record_teamfile_1()]}
                            end),

                meck:expect(erlcloud_s3, get_object,
                           fun(_Bucket, _Akey, _S3) ->
                                   erlang:error({aws_error,
                                                 {http_error,404,
                                                  "Not Found",
                                                  "xml message"}})
                           end),

                ErrObj = {aws_error,{http_error,404,"Not Found","xml message"}},
                ?assertEqual(ErrObj, hs_teamfile:get_file(TeamFileId)),

                ?assert(meck:validate(postgres_pool)),
                ?assertNot(meck:validate(erlcloud_s3))
        end
       },

       {"データベース上にファイル情報が存在しない",
        fun() ->
                Data = <<"this is file 1">>,
                TeamFile1 = record_teamfile_1(),
                TeamFile2 = TeamFile1#teamfile{contents = Data},
                TeamFileId = TeamFile2#teamfile.id,

                meck:expect(postgres_pool, equery, 
                            fun(_, _, P) -> 
                                    ?assertEqual([TeamFileId], P),
                                    {error, not_found}
                            end),

                ?assertEqual({error, not_found}, 
                             hs_teamfile:get_file(TeamFileId)),

                ?assert(meck:validate(postgres_pool))
        end
       }       

      ]
     }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup() ->
    meck:new(postgres_pool),
    meck:new(erlcloud_s3),
    meck:expect(erlcloud_s3, new, fun(_, _) -> s3_config() end),
    meck:expect(erlcloud_s3, create_bucket, fun(_Bucket, _S3) -> ok end).

start_server() ->
    {ok, _} = hs_file:start_link().

teardown() ->
    hs_file:stop(),
    meck:unload(postgres_pool),
    meck:unload(erlcloud_s3).

%% 渡されたValを含むS3オブジェクトを返す
s3_object(Val) when is_binary(Val) ->
    Len = integer_to_list(length(binary_to_list(Val))),
    [{etag,"\"aaaaaaaaaaaaaaaaaaaaaaa\""},
     {content_length,Len},
     {content_type,"application/octet_stream"},
     {delete_marker,false},
     {version_id,"null"},
     {content,Val}].

s3_config() ->
    {aws_config,"ec2.amazonaws.com","s3.amazonaws.com",80,
     "sdb.amazonaws.com","elasticloadbalancing.amazonaws.com",
     "queue.amazonaws.com","mechanicalturk.amazonaws.com",
     "monitoring.amazonaws.com","ACCESS_KEY", "SECRET_ACCESS_KEY"}.
