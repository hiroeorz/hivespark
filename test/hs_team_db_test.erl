-module(hs_team_db_test).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, [pgdatetime_to_seconds/1]).
-import(hs_mock, [pg_team_fields_tuples/0,
                  pg_record_team_1/0, record_team_1/0,
                  pg_record_team_2/0, record_team_2/0,
                  pg_record_team_3/0, record_team_3/0]).

q_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"1件のレコードをデータベースから取得した値のパース",
        fun() ->
                Sql = "select * from teams limit 1 order by id",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_1()]}
                            end),

                {ok, [Team]} = hs_team_db:q(Sql),

                ?assertEqual(record_team_1(), Team),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"3件のレコードをデータベースから取得した値のパース",
        fun() ->
                Sql = "select * from teams limit 3 order by id",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_1(),
                                      pg_record_team_2(),
                                      pg_record_team_3()]}
                            end),

                {ok, [Team1, Team2, Team3]} = hs_team_db:q(Sql),

                ?assertEqual(record_team_1(), Team1),
                ?assertEqual(record_team_2(), Team2),
                ?assertEqual(record_team_3(), Team3),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.

all_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"default",
        fun() ->
                Sql = "select * from teams order by id desc",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_3(),
                                      pg_record_team_2(),
                                      pg_record_team_1()]}
                            end),

                [Team3, Team2, Team1] = hs_team_db:all(),

                ?assertEqual(record_team_3(), Team3),
                ?assertEqual(record_team_2(), Team2),
                ?assertEqual(record_team_1(), Team1),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"empty",
        fun() ->
                Sql = "select * from teams order by id desc",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_team_fields_tuples(), []}
                            end),

                [] = hs_team_db:all(),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.

statuses_list_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"status = 1",
        fun() ->
                Sql = "select * from teams where status = $1
                          order by id desc limit $2",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([1, 100], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_1()]}
                            end),

                [Team1] = hs_team_db:statuses_list(1, 100),

                ?assertEqual(record_team_1(), Team1),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"status = 0",
        fun() ->
                Sql = "select * from teams where status = $1
                          order by id desc limit $2",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([0, 100], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_3(),
                                      pg_record_team_2()]}
                            end),

                [Team3, Team2] = hs_team_db:statuses_list(0, 100),

                ?assertEqual(record_team_3(), Team3),
                ?assertEqual(record_team_2(), Team2),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"status = 0 count = 1",
        fun() ->
                Sql = "select * from teams where status = $1
                          order by id desc limit $2",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([0, 1], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_3()]}
                            end),

                [Team3] = hs_team_db:statuses_list(0, 1),

                ?assertEqual(record_team_3(), Team3),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"empty",
        fun() ->
                Sql = "select * from teams where status = $1
                          order by id desc limit $2",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([0, 100], P),
                                    {ok, pg_team_fields_tuples(), []}
                            end),

                [] = hs_team_db:statuses_list(0, 100),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.

list_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"integer arguments",
        fun() ->
                Sql = "select * from teams where id in (2,3)",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_2(), pg_record_team_3()]}
                            end),

                {ok, [Team2, Team3]} = hs_team_db:list([2, 3]),

                ?assertEqual(record_team_2(), Team2),
                ?assertEqual(record_team_3(), Team3),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"binary arguments",
        fun() ->
                Sql = "select * from teams where id in (2,3)",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_2(), pg_record_team_3()]}
                            end),

                {ok, [Team2, Team3]} = hs_team_db:list([<<"2">>, <<"3">>]),

                ?assertEqual(record_team_2(), Team2),
                ?assertEqual(record_team_3(), Team3),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"string arguments",
        fun() ->
                Sql = "select * from teams where id in (2,3)",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_2(), pg_record_team_3()]}
                            end),

                {ok, [Team2, Team3]} = hs_team_db:list(["2", "3"]),

                ?assertEqual(record_team_2(), Team2),
                ?assertEqual(record_team_3(), Team3),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"empty arguments",
        fun() ->
                {ok, []} = hs_team_db:list([])
        end
       }

      ]
     }
    }.
