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

send(ParamList, _Req, _State, SessionKey) ->
    TextBin = proplists:get_value(<<"text">>, ParamList),
    Usr = hs_session:get_usr(SessionKey),

    Reply = 
        case hs_message:save(#message{usr_id = Usr#usr.id, text = TextBin}) of
            {ok, Message} ->
                TMsg = hs_message:to_tuple(Message),
                {[{<<"result">>, <<"success">>}, {message, TMsg}]};
            {error, Reason} ->
                {[{<<"result">>, <<"failure">>}, 
                  {<<"reason">>, list_to_binary(atom_to_list(Reason))}]}
        end,
    
    {200, jiffy:encode(Reply)}.    

%%%===================================================================
%%% Internal functions
%%%===================================================================
