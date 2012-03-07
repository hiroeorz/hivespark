%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2012 by Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%%-------------------------------------------------------------------
-module(hs_websocket_handler).
-behaviour(cowboy_http_websocket_handler).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

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
    {ok, Req2, #http_state{}, hibernate}.

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
    ?debugVal(Msg),
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
-spec handle(Controller, Action, ParamList, Req, State) -> 
                    {reply, {text, Json}, Req, NewState, hibernate} when
      Controller :: binary(),
      Action :: binary(),
      ParamList :: [tuple()],
      Req :: tuple(),
      State :: tuple(),
      NewState :: tuple(),
      Json :: binary().
handle(<<"socket">>, <<"init">>, _ParamList, Req, State) ->
    Json = jiffy:encode({[{<<"controller">>, <<"socket_handler">>}, 
                          {<<"action">>, <<"opened">>},
                          {<<"status">>, <<"accepted">>}, 
                          {<<"message">>, <<"接続完了">>}]}),
    {reply, {text, Json}, Req, State, hibernate};

handle(C, A, ParamList, Req, State) when ?AUTHENTICATED_ROUTE ->
    UsrId = proplists:get_value(<<"usr_id">>, ParamList),
    SessionKey = proplists:get_value(<<"session_key">>, ParamList),
    Loggedin = hs_session:check_loggedin(UsrId, SessionKey),

    case Loggedin of
        true -> 
            M = list_to_atom(lists:flatten([binary_to_list(C), "_controller"])),
            F = list_to_atom(binary_to_list(A)),
            {_, Bin, NewState} = M:F(ParamList, Req, State, SessionKey),
            {reply, {text, Bin}, Req, NewState, hibernate};
        false ->
            Json = jiffy:encode({[{status, <<"not_authenticated">>}]}),
            {reply, {text, Json}, Req, State, hibernate}
    end;

handle(C, A, ParamList, Req, State) when ?ROUTE ->
    M = list_to_atom(lists:flatten([binary_to_list(C), "_controller"])),
    F = list_to_atom(binary_to_list(A)),
    {_, _, Bin, NewState} = M:F(ParamList, Req, State),
    {reply, {text, Bin}, Req, NewState, hibernate};

handle(C, A, _, Req, State) ->
    io:format("invalid route ~p:~p", [C, A]),
    Json = jiffy:encode({[{result, <<"error">>}, {reason, <<"not found">>}]}),
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
