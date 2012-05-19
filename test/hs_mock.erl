-module(hs_mock).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

-import(hs_util, [pgdatetime_to_seconds/1]).

-export([pg_usr_fields_tuples/0,
         pg_record_usr_1/0, record_usr_1/0,
         pg_record_usr_2/0, record_usr_2/0,
         pg_record_usr_3/0, record_usr_3/0]).

-export([pg_team_fields_tuples/0,
         pg_record_team_1/0, record_team_1/0, 
         pg_record_team_2/0, record_team_2/0,
         pg_record_team_3/0, record_team_3/0]).

pg_usr_fields_tuples() ->
    [{column, <<"id">>, 0, 0, 0, 0}, 
     {column, <<"name">>, 0, 0, 0, 0}, 
     {column, <<"longname">>, 0, 0, 0, 0}, 
     {column, <<"email">>, 0, 0, 0, 0},
     {column, <<"password">>, 0, 0, 0, 0}, 
     {column, <<"password_seed">>, 0, 0, 0, 0}, 
     {column, <<"icon_url">>, 0, 0, 0, 0}, 
     {column, <<"lat">>, 0, 0, 0, 0}, 
     {column, <<"lng">>, 0, 0, 0, 0}, 
     {column, <<"description">>, 0, 0, 0, 0}, 
     {column, <<"created_at">>, 0, 0, 0, 0}].

pg_record_usr_1() ->
    {1, "shin", "HIROE Shin", "shin@mail.com", "123",
     "seed", "http://icon.com", "123.45", "23.45",
     "I am programmer.", {{2012,5,18}, {23,55,4.0}}}.
        
record_usr_1() ->
    #usr{id = 1, name = "shin", 
         longname = "HIROE Shin",
         email = "shin@mail.com", 
         password = "123",
         password_seed = "seed", 
         icon_url = "http://icon.com",
         lat = "123.45", lng = "23.45",
         description = "I am programmer.",
         created_at = pgdatetime_to_seconds({{2012,5,18}, {23,55,4.0}})}.

pg_record_usr_2() ->
    {2, "goro", "HIROE Goro", "goro@mail.com", "goro123",
     "goroseed", "http://icon.com/goro", "123.456", "23.456",
     "I am cat.", {{2012,5,19}, {23,56,4.0}}}.

record_usr_2() ->
    #usr{id = 2, name = "goro", 
         longname = "HIROE Goro",
         email = "goro@mail.com", 
         password = "goro123",
         password_seed = "goroseed", 
         icon_url = "http://icon.com/goro",
         lat = "123.456", lng = "23.456",
         description = "I am cat.",
         created_at = pgdatetime_to_seconds({{2012,5,19}, {23,56,4.0}})}.

pg_record_usr_3() ->
    {3, "sakura", "HIROE Sakura", "sakura@mail.com", "sakura123",
     "sakuraseed", "http://icon.com/sakura", "123.4", "23.4",
     "I am small cat.", {{2011,5,20}, {22,54,3.0}}}.

record_usr_3() ->
    #usr{id = 3, name = "sakura", 
         longname = "HIROE Sakura",
         email = "sakura@mail.com", 
         password = "sakura123",
         password_seed = "sakuraseed", 
         icon_url = "http://icon.com/sakura",
         lat = "123.4", lng = "23.4",
         description = "I am small cat.",
         created_at = pgdatetime_to_seconds({{2011,5,20}, {22,54,3.0}})}.

pg_team_fields_tuples() ->
    [{column, <<"id">>, 0, 0, 0, 0}, 
     {column, <<"name">>, 0, 0, 0, 0}, 
     {column, <<"owner_id">>, 0, 0, 0, 0},
     {column, <<"icon_url">>, 0, 0, 0, 0}, 
     {column, <<"description">>, 0, 0, 0, 0},
     {column, <<"status">>, 0, 0, 0, 0}, 
     {column, <<"status_description">>, 0, 0, 0, 0},
     {column, <<"created_at">>, 0, 0, 0, 0}].

pg_record_team_1() ->
    {1, "team1", 1, "http://icon.com/team1","Good Team.", 
     1, "Now Developping.",
     {{2012,5,18}, {23,55,4.0}}}.

pg_record_team_2() ->
    {2, "team2", 3, "http://icon.com/team2","Fuu.", 
     0, "Now Stopping.",
     {{2012,5,19}, {23,55,5.0}}}.

pg_record_team_3() ->
    {3, "team3", 3, "http://icon.com/team3","Hoge.", 
     0, "Now Stopping?",
     {{2012,5,20}, {23,55,5.0}}}.

record_team_1() ->
    #team{id = 1, 
          name = "team1", 
          owner_id = 1,
          icon_url = "http://icon.com/team1",
          description = "Good Team.",
          status = 1,
          status_description = "Now Developping.",
          created_at = pgdatetime_to_seconds({{2012,5,18}, {23,55,4.0}})}.

record_team_2() ->
    #team{id = 2, 
          name = "team2", 
          owner_id = 3,
          icon_url = "http://icon.com/team2",
          description = "Fuu.",
          status = 0,
          status_description = "Now Stopping.",
          created_at = pgdatetime_to_seconds({{2012,5,19}, {23,55,5.0}})}.

record_team_3() ->
    #team{id = 3, 
          name = "team3", 
          owner_id = 3,
          icon_url = "http://icon.com/team3",
          description = "Hoge.",
          status = 0,
          status_description = "Now Stopping?",
          created_at = pgdatetime_to_seconds({{2012,5,20}, {23,55,5.0}})}.
