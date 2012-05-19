-module(hs_usr_db_test).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, [pgdatetime_to_seconds/1]).
-import(hs_mock, [pg_usr_fields_tuples/0,
                  pg_record_usr_1/0, record_usr_1/0,
                  pg_record_usr_2/0, record_usr_2/0,
                  pg_record_usr_3/0, record_usr_3/0]).

q_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"1件のレコードをデータベースから取得した値のパース",
        fun() ->
                Sql = "select * from usrs limit 1 order by id",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(S, Sql),
                                    ?assertEqual([], P),
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1()]}
                            end),

                {ok, UsrList} = hs_usr_db:q(Sql),

                ?assertEqual(1, length(UsrList)),
                ?assertEqual(record_usr_1(), lists:nth(1, UsrList)),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"3件のレコードをデータベースから取得した値のパース",
        fun() ->
                Sql = "select * from usrs limit 1 order by id",
                meck:expect(postgres_pool, equery, 
                            fun(_, S, P) -> 
                                    ?assertEqual(Sql, S),
                                    ?assertEqual([], P),
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1(), 
                                      pg_record_usr_2(),
                                      pg_record_usr_3()]}
                            end),

                {ok, UsrList} = hs_usr_db:q(Sql),

                ?assertEqual(3, length(UsrList)),
                ?assertEqual(record_usr_1(), lists:nth(1, UsrList)),
                ?assertEqual(record_usr_2(), lists:nth(2, UsrList)),
                ?assertEqual(record_usr_3(), lists:nth(3, UsrList)),
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
                meck:expect(postgres_pool, equery, 
                            fun(_, Sql, Params) ->
                                    ?assertEqual("select * from usrs order by id desc limit $1", Sql),
                                    ?assertEqual([3], Params),
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1(),
                                      pg_record_usr_2(),
                                      pg_record_usr_3()]}
                            end),

                {ok, [Usr1, Usr2, Usr3]} = hs_usr_db:all(3),

                ?assertEqual(record_usr_1(), Usr1),
                ?assertEqual(record_usr_2(), Usr2),
                ?assertEqual(record_usr_3(), Usr3),
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
       {"default",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, Sql, Params) ->
                                    ?assertEqual("select * from usrs where id in (1,3)", Sql),
                                    ?assertEqual([], Params),
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1(),
                                      pg_record_usr_3()]}
                            end),

                {ok, [Usr1, Usr3]} = hs_usr_db:list([1, 3]),

                ?assertEqual(record_usr_1(), Usr1),
                ?assertEqual(record_usr_3(), Usr3),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"binary arguments",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, Sql, Params) ->
                                    ?assertEqual("select * from usrs where id in (1,3)", Sql),
                                    ?assertEqual([], Params),
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1(),
                                      pg_record_usr_3()]}
                            end),

                {ok, [Usr1, Usr3]} = hs_usr_db:list([<<"1">>, <<"3">>]),

                ?assertEqual(record_usr_1(), Usr1),
                ?assertEqual(record_usr_3(), Usr3),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"string arguments",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, Sql, Params) ->
                                    ?assertEqual("select * from usrs where id in (1,3)", Sql),
                                    ?assertEqual([], Params),
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1(),
                                      pg_record_usr_3()]}
                            end),

                {ok, [Usr1, Usr3]} = hs_usr_db:list(["1", "3"]),

                ?assertEqual(record_usr_1(), Usr1),
                ?assertEqual(record_usr_3(), Usr3),
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
       {"default",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1()]}
                            end),

                UsrRecord = record_usr_1(),
                {ok, Usr1} = hs_usr_db:insert(<<"shin">>,
                                              <<"HIROE Shin">>,
                                              <<"shin@mail.com">>,
                                              <<"123">>,
                                              <<"http://icon.com">>,
                                              <<"I am programmer.">>),

                ?assertEqual(UsrRecord, Usr1),
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
       {"default",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1()]}
                            end),

                {ok, Usr} = hs_usr_db:lookup_id(1),

                ?assertEqual(record_usr_1(), Usr),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"binary argument",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1()]}
                            end),

                {ok, Usr} = hs_usr_db:lookup_id(<<"1">>),

                ?assertEqual(record_usr_1(), Usr),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"list argument",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1()]}
                            end),

                {ok, Usr} = hs_usr_db:lookup_id("1"),

                ?assertEqual(record_usr_1(), Usr),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"not found",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), []}
                            end),

                {error, not_found} = hs_usr_db:lookup_id(99),
                ?assert(meck:validate(postgres_pool))
        end
       }


      ]
     }
    }.

lookup_name_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"default",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), 
                                     [pg_record_usr_1()]}
                            end),

                {ok, Usr} = hs_usr_db:lookup_name("shin"),
                ?assertEqual(record_usr_1(), Usr),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"not found",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), []}
                            end),

                {error, not_found} = hs_usr_db:lookup_name("not_exit_user"),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.

update_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(postgres_pool) end, 
      fun(_) -> meck:unload(postgres_pool) end,
      [
       {"default",
        fun() ->
                R = record_usr_1(),
                R1 = R#usr{name = "shin1",
                           longname = "HIROE Shin1",
                           email = "shin1@mail.com",
                           icon_url = "http://icon.com/shin1",
                           description = "I am Programmer 1.",
                           lat = "123.123",
                           lng = "234.56"},

                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    PgR1 = {R1#usr.id, 
                                            R1#usr.name, 
                                            R1#usr.longname, 
                                            R1#usr.email, 
                                            R1#usr.password, 
                                            R1#usr.password_seed, 
                                            R1#usr.icon_url, 
                                            R1#usr.lat, R1#usr.lng,
                                            R1#usr.description, 
                                            {{2012,5,18}, {23,55,4.0}}},
                                    {ok, pg_usr_fields_tuples(), [PgR1]}
                            end),

                {ok, Usr} = hs_usr_db:update(R1),
                ?assertEqual(R1, Usr),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"not found",
        fun() ->
                R = record_usr_1(),
                R1 = R#usr{name = "shin1",
                           longname = "HIROE Shin1",
                           email = "shin1@mail.com",
                           icon_url = "http://icon.com/shin1",
                           description = "I am Programmer 1.",
                           lat = "123.123",
                           lng = "234.56"},

                meck:expect(postgres_pool, equery, 
                            fun(_, _S, _P) ->
                                    {ok, pg_usr_fields_tuples(), []}
                            end),

                {error, not_found} = hs_usr_db:update(R1),
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
       {"default",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, Sql, Params) -> 
                                    ?assertEqual("delete from usrs where id = $1", Sql),
                                    ?assertEqual([1], Params),
                                    {ok, 0} 
                            end),

                {ok, deleted} = hs_usr_db:delete(1),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"binary argument",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, Sql, Params) -> 
                                    ?assertEqual("delete from usrs where id = $1", Sql),
                                    ?assertEqual([1], Params),
                                    {ok, 0} 
                            end),

                {ok, deleted} = hs_usr_db:delete(<<"1">>),
                ?assert(meck:validate(postgres_pool))
        end
       },

       {"list argument",
        fun() ->
                meck:expect(postgres_pool, equery, 
                            fun(_, Sql, Params) -> 
                                    ?assertEqual("delete from usrs where id = $1", Sql),
                                    ?assertEqual([1], Params),
                                    {ok, 0} 
                            end),

                {ok, deleted} = hs_usr_db:delete("1"),
                ?assert(meck:validate(postgres_pool))
        end
       }

      ]
     }
    }.
