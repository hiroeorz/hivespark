-module(hs_message_db_test).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, [pgdatetime_to_seconds/1]).
-import(hs_mock, [pg_message_fields_tuples/0, 
                  pg_record_message_1/0, record_message_1/0,
                  pg_record_message_2/0, record_message_2/0]).

q_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"1件のレコードをデータベースから取得した値のパース",
        fun() ->
                Sql = "select * from messages limit 1 order by id",

                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_message_fields_tuples(), 
                                     [pg_record_message_1()]}
                            end),

                {ok, [Message]} = hs_message_db:q(Sql),
                ?assertEqual(record_message_1(), Message),
                ?assert(meck:validate(postgres_pool))
        end
       }
      ]
     }
    }.

insert_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"1件のメッセージをデータベースに保存して結果を取得する",
        fun() ->

                meck:expect(postgres_pool, equery, 
                            fun(_, _S, P) -> 
                                    ?assertMatch(
                                       [2,3,"hello world", {{_,_,_},{_,_,_}},
                                        "11.111","22.222",null,null], P),
                                    {ok, pg_message_fields_tuples(), 
                                     [pg_record_message_1()]}
                            end),

                Message = record_message_1(),
                {ok, Message} = hs_message_db:insert(Message),
                ?assert(meck:validate(postgres_pool))
        end
       }
      ]
     }
    }.

get_meg_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"メッセージIDを指定して1件のメッセージを取得する",
        fun() ->
                Sql = "select * from messages where id = $1",
                MsgId = 1,
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([MsgId], P),
                                    {ok, pg_message_fields_tuples(), 
                                     [pg_record_message_1()]}
                            end),

                {ok, Message} = hs_message_db:get_msg(MsgId),
                ?assertEqual(record_message_1(), Message),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"存在しないメッセージでは{error, not_found}を返す",
        fun() ->
                Sql = "select * from messages where id = $1",
                MsgId = 999,
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([MsgId], P),
                                    {ok, pg_message_fields_tuples(), []}
                            end),

                {error, not_found} = hs_message_db:get_msg(MsgId),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"データベースでエラー発生時にはでは{error, Reason}を返す",
        fun() ->
                Sql = "select * from messages where id = $1",
                MsgId = 999,
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([MsgId], P),
                                    {error, connection_error}
                            end),

                {error, connection_error} = hs_message_db:get_msg(MsgId),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.

list_of_team_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"チームに投稿されたメッセージを取得する",
        fun() ->
                TeamId = 2,
                Count = 5,
                Offset = 0,
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, P) -> 
                                    ?assertEqual([TeamId, Count, Offset], P),
                                    {ok, pg_message_fields_tuples(), 
                                     [pg_record_message_1(),
                                      pg_record_message_2()]}
                            end),

                {ok, [Message1, Message2]} = 
                    hs_message_db:list_of_team(TeamId, Offset, Count),

                ?assertEqual(record_message_1(), Message1),
                ?assertEqual(record_message_2(), Message2),
                ?assert(meck:validate(postgres_pool))
        end
       }
      ]
     }
    }.

list_of_team_by_since_id_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"チームに投稿されたメッセージのうち、SiceId以降のものを取得する",
        fun() ->
                TeamId = 2,
                SinceId = 2,
                meck:expect(postgres_pool, equery,
                            fun(_, _S, P) -> 
                                    ?assertEqual([TeamId, SinceId], P),
                                    {ok, pg_message_fields_tuples(), 
                                     [pg_record_message_1(),
                                      pg_record_message_2()]}
                            end),

                {ok, [Message1, Message2]} = 
                    hs_message_db:list_of_team_by_since_id(TeamId, SinceId),

                ?assertEqual(record_message_1(), Message1),
                ?assertEqual(record_message_2(), Message2),
                ?assert(meck:validate(postgres_pool))
        end
       }
      ]
     }
    }.

list_of_usr_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"特定のユーザの投稿したメッセージを取得する",
        fun() ->
                UsrId = 1,
                Count = 2,
                meck:expect(postgres_pool, equery,
                            fun(_, _S, P) -> 
                                    ?assertEqual([UsrId, Count], P),
                                    {ok, pg_message_fields_tuples(), 
                                     [pg_record_message_1(),
                                      pg_record_message_2()]}
                            end),

                {ok, [Message1, Message2]} = 
                    hs_message_db:list_of_usr(UsrId, Count),

                ?assertEqual(record_message_1(), Message1),
                ?assertEqual(record_message_2(), Message2),
                ?assert(meck:validate(postgres_pool))
        end
       }
      ]
     }
    }.

get_latest_of_team_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"特定のチームに投稿された最後のメッセージを返す",
        fun() ->
                TeamId = 1,
                meck:expect(postgres_pool, equery,
                            fun(_, _S, P) -> 
                                    ?assertEqual([TeamId], P),
                                    {ok, pg_message_fields_tuples(), 
                                     [pg_record_message_1()]}
                            end),

                Message1 = hs_message_db:get_latest_of_team(TeamId),
                ?assertEqual(record_message_1(), Message1),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"メッセージが１件も投稿されていないチームであればundefinedを返す",
        fun() ->
                TeamId = 1,
                meck:expect(postgres_pool, equery,
                            fun(_, _S, P) -> 
                                    ?assertEqual([TeamId], P),
                                    {ok, pg_message_fields_tuples(), []}
                            end),

                undefined = hs_message_db:get_latest_of_team(TeamId),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.
