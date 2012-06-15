-module(hs_file_test).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, []).
-import(hs_mock, []).

start_link_test_() ->
    {spawn,
     {setup, 
      fun()  -> setup() end, 
      fun(_) -> teardown() end,
      [
       {"サーバーの起動",
        fun() ->
                ?assertMatch({ok, _}, hs_file:start_link()),
                ?assert(meck:validate(erlcloud_s3))
        end
       }
      ]
     }
    }.

save_test_() ->
    {spawn,
     {setup, 
      fun()  -> setup(), start_server() end, 
      fun(_) -> teardown() end,
      [
       {"データの保存に成功",
        fun() ->
                Key = "name",
                Val = <<"Mike">>,

                meck:expect(erlcloud_s3, put_object,
                           fun(_Bucket, Akey, Aval, _S3) ->
                                   ?assertEqual(Akey, Key),
                                   ?assertEqual(Aval, Val),
                                   [{version_id, "null"}]
                           end),

                ?assertMatch({ok, _}, hs_file:save(Key, Val)),
                ?assert(meck:validate(erlcloud_s3))
        end
       },

       {"データの保存に失敗",
        fun() ->
                Key = "name",
                Val = <<"Mike">>,

                meck:expect(erlcloud_s3, put_object,
                           fun(_Bucket, _Akey, _Aval, _S3) ->
                                   erlang:error({s3_error, 500, "error"})
                           end),

                ?assertMatch({s3_error, _, _}, hs_file:save(Key, Val)),
                ?assertNot(meck:validate(erlcloud_s3))
        end
       }

      ]
     }
    }.

read_test_() ->
    {spawn,
     {setup, 
      fun()  -> setup(), start_server() end, 
      fun(_) -> teardown() end,
      [
       {"データの読み込みに成功",
        fun() ->
                Key = "name",
                Val = <<"This is contents">>,
                S3_Obj = s3_object(<<"This is contents">>),

                meck:expect(erlcloud_s3, get_object,
                           fun(_Bucket, Akey, _S3) ->
                                   ?assertEqual(Akey, Key),
                                   S3_Obj
                           end),

                ?assertEqual({ok, Val, S3_Obj}, hs_file:read(Key)),
                ?assert(meck:validate(erlcloud_s3))
        end
       },

       {"データが存在しない",
        fun() ->
                Key = "name",

                meck:expect(erlcloud_s3, get_object,
                           fun(_Bucket, _Akey, _S3) ->
                                   erlang:error({aws_error,
                                                 {http_error,404,
                                                  "Not Found",
                                                  "xml message"}})
                           end),

                ?assertMatch({aws_error, _}, hs_file:read(Key)),
                ?assertNot(meck:validate(erlcloud_s3))
        end
       }

      ]
     }
    }.

delete_test_() ->
    {spawn,
     {setup, 
      fun()  -> setup(), start_server() end, 
      fun(_) -> teardown() end,
      [
       {"データの削除に成功",
        fun() ->
                Key = "name",

                meck:expect(erlcloud_s3, delete_object,
                           fun(_Bucket, Akey, _S3) ->
                                   ?assertEqual(Akey, Key),
                                   [{delete_marker, false}, {version_id, "null"}]
                           end),

                ?assertMatch({ok, _}, hs_file:delete(Key)),
                ?assert(meck:validate(erlcloud_s3))
        end
       }

      ]
     }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup() ->
    meck:new(erlcloud_s3),
    meck:expect(erlcloud_s3, new, fun(_, _) -> s3_config() end),
    meck:expect(erlcloud_s3, create_bucket, fun(_Bucket, _S3) -> ok end).

start_server() ->
    {ok, _} = hs_file:start_link().

teardown() ->
    hs_file:stop(),
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

