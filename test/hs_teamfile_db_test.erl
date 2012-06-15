-module(hs_teamfile_db_test).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, [pgdatetime_to_seconds/1]).
-import(hs_mock, [pg_teamfile_fields_tuples/0,
                  pg_record_teamfile_1/0, record_teamfile_1/0,
                  pg_record_teamfile_2/0, record_teamfile_2/0,
                  pg_record_teamfile_3/0, record_teamfile_3/0]).

q_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"1件のレコードをデータベースから取得した値のパース",
        fun() ->
                Sql = "select * from teamfiles limit 1 order by id",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_teamfile_fields_tuples(), 
                                     [pg_record_teamfile_1()]}
                            end),

                {ok, [TeamFile]} = hs_teamfile_db:q(Sql),

                ?assertEqual(record_teamfile_1(), TeamFile),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"3件のレコードをデータベースから取得した値のパース",
        fun() ->
                Sql = "select * from teamfiles limit 3 order by id",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_teamfile_fields_tuples(), 
                                     [pg_record_teamfile_1(),
                                      pg_record_teamfile_2(),
                                      pg_record_teamfile_3()]}
                            end),

                {ok, [TeamFile1, TeamFile2, TeamFile3]} = hs_teamfile_db:q(Sql),

                ?assertEqual(record_teamfile_1(), TeamFile1),
                ?assertEqual(record_teamfile_2(), TeamFile2),
                ?assertEqual(record_teamfile_3(), TeamFile3),
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
       {"新規ファイル情報の保存",
        fun() ->
                TeamId = 1, 
                OwnerId = 2, 
                Name = "document.pdf", 
                Description = <<"this is file 1">>,

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

                {ok, TeamFile1} = hs_teamfile_db:insert(TeamId, 
                                                        OwnerId, 
                                                        Name, 
                                                        Description),

                ?assertEqual(record_teamfile_1(), TeamFile1),
                ?assert(meck:validate(postgres_pool))
        end
       }
      ]
     }
    }.

delete_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"新規ファイル情報の保存",
        fun() ->
                TeamFileId = 1,
                meck:expect(postgres_pool, equery, 
                            fun(_, _, P) -> 
                                    ?assertMatch([TeamFileId], P),
                                    {ok, 0}
                            end),

                {ok, deleted} = hs_teamfile_db:delete(TeamFileId),
                ?assert(meck:validate(postgres_pool))
        end
       }
      ]
     }
    }.

lookup_id_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"与えられたIdに対応するファイル情報をかえす",
        fun() ->
                TeamFileId = 1,
                meck:expect(postgres_pool, equery, 
                            fun(_, _, P) -> 
                                    ?assertEqual([TeamFileId], P),
                                    {ok, pg_teamfile_fields_tuples(), 
                                     [pg_record_teamfile_1()]}
                            end),

                {ok, TeamFile} = hs_teamfile_db:lookup_id(TeamFileId),

                ?assertEqual(record_teamfile_1(), TeamFile),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"与えられたIdに対応するファイル情報が無かった場合",
        fun() ->
                TeamFileId = -99,
                meck:expect(postgres_pool, equery, 
                            fun(_, _, P) -> 
                                    ?assertEqual([TeamFileId], P),
                                    {ok, pg_teamfile_fields_tuples(), []}
                            end),

                {error, not_found} = hs_teamfile_db:lookup_id(TeamFileId),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.
