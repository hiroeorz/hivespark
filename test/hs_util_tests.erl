-module(hs_util_tests).

-include_lib("eunit/include/eunit.hrl").

-import(hs_util, [pgdatetime_to_seconds/1]).

pgdatetime_to_seconds_test_() ->
    {inorder,
     {setup, fun() -> ok end, fun(_) -> ok end,

      [
       {"default",
        fun() ->
                ?assertEqual(63113904000,
                             pgdatetime_to_seconds({{2000, 1, 1},
                                                     { 0, 0, 0.0}})),
                ?assertEqual(63113904001,
                             pgdatetime_to_seconds({{2000, 1, 1},
                                                    { 0, 0, 1.0}}))
        end
       }

      ]
     }
    }.
