-module(hs_usr_cache_test).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, []).
-import(hs_mock, []).

store_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(eredis_pool) end, 
      fun(_) -> meck:unload(eredis_pool) end,
      [
       {"ユーザ情報をキャッシュに保存する",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HSET", _, UsrId, _UsrBin]) 
                                  when is_integer(UsrId) ->
                                    ?assertEqual(Usr#usr.id, UsrId),
                                    {ok, <<"1">>};
                            
                               (?DB_SRV, ["HSET", _, Name, UsrId]) 
                                  when is_list(Name) ->
                                    ?assertEqual(Usr#usr.id, UsrId),
                                    ?assertEqual(Usr#usr.name, Name),
                                    {ok, <<"1">>}
                            end),

                {ok, Usr} = hs_usr_cache:store(Usr)
        end
       }
      ]
     }
    }.

lookup_id_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(eredis_pool) end, 
      fun(_) -> meck:unload(eredis_pool) end,
      [
       {"キャッシュからユーザを取得する(UsrId :: integer)",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USR_DB, UsrId]) ->
                                    ?assertEqual(Usr#usr.id, 
                                        list_to_integer(binary_to_list(UsrId))),
                                    {ok, term_to_binary(Usr)}
                            end),

                {ok, Usr} = hs_usr_cache:lookup_id(1)
        end
       },

       {"キャッシュからユーザを取得する(UsrId :: list)",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USR_DB, UsrId]) ->
                                    ?assertEqual(Usr#usr.id, 
                                        list_to_integer(binary_to_list(UsrId))),
                                    {ok, term_to_binary(Usr)}
                            end),

                {ok, Usr} = hs_usr_cache:lookup_id("1")
        end
       },

       {"キャッシュからユーザを取得する(UsrId :: binary)",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USR_DB, UsrId]) ->
                                    ?assertEqual(Usr#usr.id, 
                                       list_to_integer(binary_to_list(UsrId))),
                                    {ok, term_to_binary(Usr)}
                            end),

                {ok, Usr} = hs_usr_cache:lookup_id(<<"1">>)
        end
       },

       {"存在しなければ{error, not_found}を返す",
        fun() ->
                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USR_DB, _UsrId]) ->
                                    {ok, undefined}
                            end),

                {error, not_found} = hs_usr_cache:lookup_id(<<"999">>)
        end
       }

      ]
     }
    }.

lookup_name_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(eredis_pool) end, 
      fun(_) -> meck:unload(eredis_pool) end,
      [
       {"ユーザ名からユーザ情報を取得する",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USR_DB, UsrId]) ->
                                    ?assertEqual(Usr#usr.id, 
                                       list_to_integer(binary_to_list(UsrId))),
                                    {ok, term_to_binary(Usr)};

                               (?DB_SRV, ["HGET", ?USR_NAME_INDEX_KEY, Name]) ->
                                    ?assertEqual(list_to_binary(Usr#usr.name), 
                                                 Name),
                                    {ok, <<"1">>}
                            end),

                {ok, Usr} = hs_usr_cache:lookup_name("taro")
        end
       },

       {"存在しないユーザ名なら{error, not_found}を返す",
        fun() ->
                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USR_NAME_INDEX_KEY, _]) ->
                                    {ok, undefined}
                            end),

                {error, not_found} = hs_usr_cache:lookup_name("taro")
        end
       }

      ]
     }
    }.

delete_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(eredis_pool) end, 
      fun(_) -> meck:unload(eredis_pool) end,
      [
       {"ユーザを削除する(UsrId :: integer)",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HDEL", ?USR_DB, UsrId]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, <<"1">>};
                               (?DB_SRV, 
                                ["HDEL", ?USR_NAME_INDEX_KEY, _Name]) ->
                                    {ok, <<"1">>};
                               (?DB_SRV, ["HGET", ?USR_DB, _UsrId]) ->
                                    {ok, term_to_binary(Usr)}
                            end),

                {ok, deleted} = hs_usr_cache:delete(1)
        end
       },

       {"ユーザを削除する(UsrId :: list)",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HDEL", ?USR_DB, UsrId]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, <<"1">>};
                               (?DB_SRV, 
                                ["HDEL", ?USR_NAME_INDEX_KEY, _Name]) ->
                                    {ok, <<"1">>};
                               (?DB_SRV, ["HGET", ?USR_DB, _UsrId]) ->
                                    {ok, term_to_binary(Usr)}
                            end),

                {ok, deleted} = hs_usr_cache:delete("1")
        end
       },

       {"ユーザを削除する(UsrId :: binary)",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HDEL", ?USR_DB, UsrId]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, <<"1">>};
                               (?DB_SRV, 
                                ["HDEL", ?USR_NAME_INDEX_KEY, _Name]) ->
                                    {ok, <<"1">>};
                               (?DB_SRV, ["HGET", ?USR_DB, _UsrId]) ->
                                    {ok, term_to_binary(Usr)}
                            end),

                {ok, deleted} = hs_usr_cache:delete(<<"1">>)
        end
       },

       {"ユーザが存在しなければ{error, not_found}を返す",
        fun() ->
                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USR_DB, _UsrId]) ->
                                    {ok, undefined}
                            end),

                {error, not_found} = hs_usr_cache:delete(1)
        end
       },

       {"削除ユーザ数が0だった場合は{error, not_found}を返す",
        fun() ->
                Usr = #usr{id = 1,
                           name = "taro",
                           longname = "sato taro",
                           email = "toro@mail.com",
                           password = "123",
                           description = "I am taro"},

                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HDEL", ?USR_DB, UsrId]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, <<"0">>};
                               (?DB_SRV, ["HGET", ?USR_DB, _UsrId]) ->
                                    {ok, term_to_binary(Usr)}
                            end),

                {error, not_found} = hs_usr_cache:delete(1)
        end
       }

      ]
     }
    }.

add_team_id_list_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(eredis_pool) end, 
      fun(_) -> meck:unload(eredis_pool) end,
      [
       {"ユーザの所属するチームIDをキャッシュに保存する(UsrId :: integer)",
        fun() ->
                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HSET", ?USRS_TEAMS_KEY, 
                                          UsrId, _TeamIdsBin]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, <<"1">>}
                            end),

                ok = hs_usr_cache:add_team_id_list(1, [1,2,3])
        end
       },

       {"ユーザの所属するチームIDをキャッシュに保存する(UsrId :: binary)",
        fun() ->
                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HSET", ?USRS_TEAMS_KEY, 
                                          UsrId, _TeamIdsBin]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, <<"1">>}
                            end),

                ok = hs_usr_cache:add_team_id_list(<<"1">>, [1,2,3])
        end
       }

      ]
     }
    }.

get_team_id_list_test_() ->
    {spawn,
     {setup, 
      fun()  -> meck:new(eredis_pool) end, 
      fun(_) -> meck:unload(eredis_pool) end,
      [
       {"ユーザの所属するチームIDをキャッシュから取得する(UsrId :: integer)",
        fun() ->
                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USRS_TEAMS_KEY, UsrId]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, term_to_binary([1, 2, 3])}
                            end),

                [1, 2, 3] = hs_usr_cache:get_team_id_list(1)
        end
       },

       {"ユーザの所属するチームIDをキャッシュから取得する(UsrId :: binary)",
        fun() ->
                meck:expect(eredis_pool, q,
                            fun(?DB_SRV, ["HGET", ?USRS_TEAMS_KEY, UsrId]) ->
                                    ?assertEqual(<<"1">>, UsrId),
                                    {ok, term_to_binary([1, 2, 3])}
                            end),

                [1, 2, 3] = hs_usr_cache:get_team_id_list(<<"1">>)
        end
       }

      ]
     }
    }.
