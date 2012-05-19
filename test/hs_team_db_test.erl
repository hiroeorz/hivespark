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
                meck:expect(postgres_pool, equery, 
                            fun(_, _, _) -> 
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_1()]}
                            end),

                {ok, [Team]} = 
                    hs_team_db:q("select * from teams limit 1 order by id"),

                ?assertEqual(record_team_1(), Team),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"3件のレコードをデータベースから取得した値のパース",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _, _) -> 
                                    {ok, pg_team_fields_tuples(), 
                                     [pg_record_team_1(),
                                      pg_record_team_2(),
                                      pg_record_team_3()]}
                            end),

                {ok, [Team1, Team2, Team3]} = 
                    hs_team_db:q("select * from teams limit 3 order by id"),

                ?assertEqual(record_team_1(), Team1),
                ?assertEqual(record_team_2(), Team2),
                ?assertEqual(record_team_3(), Team3),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.
