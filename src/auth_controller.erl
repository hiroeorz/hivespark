%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(auth_controller).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([login/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

login(ParamList, _Req, State) ->
    Username = proplists:get_value(<<"username">>, ParamList),
    Password = proplists:get_value(<<"password">>, ParamList),
    io:format("username:~s password:~s", [Username, Password]),
    Auth = hs_usr:authenticate(Username, Password),

    case Auth of
        {ok, UsrId} ->
            {ok, Usr} = hs_usr:lookup_id(UsrId),
            {ok, SessionKey} = hs_session:create(Usr),
            RepText = lists:flatten(["ようこそ ", Usr#usr.longname, "さん！"]),

            Reply = [{status, true}, {message, list_to_binary(RepText)}, 
                     {usr_id, Usr#usr.id}, {session_key, SessionKey}],

            Cookie1 = cowboy_cookies:cookie(<<"session_key">>, SessionKey,
                                           [{path, <<"/">>}]),

            UsrIdBin = list_to_binary(integer_to_list(UsrId)),
            Cookie2 = cowboy_cookies:cookie(<<"usr_id">>, UsrIdBin,
                                           [{path, <<"/">>}]),

            {200, [Cookie1, Cookie2], jiffy:encode({Reply}), State};
        Else ->
            ?debugVal(Else),
            Reply = [{status, false}, {message, list_to_binary("認証失敗")}],
            {401, [], jiffy:encode({Reply}), State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
