%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2012 by Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%%-------------------------------------------------------------------
-module(hivespark_websocket_handler).
-behaviour(cowboy_http_websocket_handler).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([init/3, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3,
         websocket_terminate/3]).

%%%===================================================================
%% @doc HTTP CallBacks
%%%===================================================================

init({tcp, http}, Req, _Opts) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, undefined_state};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

terminate(_Req, _State) ->
    ok.

%%%===================================================================
%% @doc WebSocket CallBacks
%%%===================================================================

websocket_init(_Any, Req, []) ->
    timer:send_interval(60000, tick),
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_info(tick, Req, State) ->
    {reply, {text, <<"Tick">>}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% @doc WebSocket Handler
%%%===================================================================

websocket_handle({text, Msg}, Req, State) ->
    case jiffy:decode(Msg) of
        {ParamList} ->
            Controller = get_value(<<"controller">>, ParamList),
            Action = get_value(<<"action">>, ParamList),
            handle(Controller, Action, ParamList, Req, State);
        {error, _Reason} ->
            Reply = {text, <<"{result: 'error'}">>},
            {reply, Reply, Req, State, hibernate}
    end;

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

%%%===================================================================
%%% @doc Handler
%%%===================================================================

handle(<<"chat">>, <<"init">>, _ParamList, Req, State) ->
    Json = jiffy:encode({[{status, <<"accepted">>}, 
                          {message, <<"server accepted!">>}]}),
    {reply, {text, Json}, Req, State, hibernate};

handle(<<"message">>, <<"send">>, ParamList, Req, State) ->
    TextBin = get_value(<<"text">>, ParamList),
    RepText = lists:flatten(["sent: ", binary_to_list(TextBin)]),
    Json = jiffy:encode({[{status, true}, {message, list_to_binary(RepText)}]}),
    {reply, {text, Json}, Req, State, hibernate}.
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_value(Key, Params) -> Value when
      Key :: atom(),
      Params :: list(term()),
      Value :: string() | integer() | boolean() | tuple().
get_value(Key, Params) ->
    proplists:get_value(Key, Params).
