%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2012 by Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%%-------------------------------------------------------------------
-module(notification_handler).
-behaviour(cowboy_http_websocket_handler).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

%%%===================================================================
%% @doc 
%% initialize functions
%% @end
%%%===================================================================

init({tcp, http}, _Req, _Opts) ->
        {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    case hs_session:check_loggedin_with_req(Req) of
        true ->
            SessionKey = hs_session:get_session_key_with_req(Req),
            Usr = hs_session:get_usr(SessionKey),
            ok = hs_usr_cache:add_worker_pid(Usr#usr.id, self()),
            io:format("websocket init ok~n"),
            {ok, Req, undefined_state};
        false ->
            {shutdown, Req}
    end.

%%%===================================================================
%% @doc
%% called when request received from http client.
%% @end
%%%===================================================================
%% 一定時間通信が無いと接続を遮断するルータ対策。ブラウザと１分に１回やりとりする。
websocket_handle({text, <<"TICK">>}, Req, State) ->
    ?debugVal("TICK"),
    Reply = jiffy:encode({[{<<"type">>, <<"keep_alive">>}, 
                           {<<"data">>, <<"TACK">>}]}),
    {reply, {text, Reply}, Req, State};

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%%%===================================================================
%% @doc
%% called when message received from other process.
%% @end
%%%===================================================================
websocket_info({notification, _From, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%%%===================================================================
%% @doc
%% called when client closed connection.
%% @end
%%%===================================================================
websocket_terminate(_Reason, Req, _State) ->
    SessionKey = hs_session:get_session_key_with_req(Req),
    Usr = hs_session:get_usr(SessionKey),
    ok = hs_usr_cache:delete_worker_pid(Usr#usr.id, self()),
    io:format("websocket terminated~n"),
    ok.
