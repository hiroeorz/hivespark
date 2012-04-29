-module(hs_util_tests).

-include_lib("eunit/include/eunit.hrl").

-import(hs_util, [create_datetime_string/1]).

create_datetime_string_test_() ->
    [
        {"default",
            ?_assertEqual(<<"2000-01-01 00:00:00">>,
                          create_datetime_string({{2000,1,1},{0,0,0}}))},
        {"default with milli-sec ?",
            ?_assertEqual(<<"2000-01-01 00:00:00">>,
                          create_datetime_string({{2000,1,1},{0,0,0,0}}))}
    ].
