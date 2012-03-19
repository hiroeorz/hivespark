%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(message_controller).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([send/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

send(ParamList, _Req, State, SessionKey) ->
    TextBin = proplists:get_value(<<"text">>, ParamList),
    TeamId = proplists:get_value(<<"team_id">>, ParamList),

    Usr = hs_session:get_usr(SessionKey),
    Msg = #message{usr_id = Usr#usr.id, text = TextBin, team_id=TeamId},

    Reply = 
        case hs_message:save(Msg) of
            {ok, Message} ->
                TMsg = hs_message:to_tuple(Message),
                {[{<<"result">>, <<"success">>}, {message, TMsg}]};
            {error, Reason} ->
                {[{<<"result">>, <<"failure">>}, 
                  {<<"reason">>, list_to_binary(atom_to_list(Reason))}]}
        end,
    
    {200, jiffy:encode(Reply), State}.    

%%%===================================================================
%%% Internal functions
%%%===================================================================
