%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(hivespark_http_handler).
-behaviour(cowboy_http_handler).

%% Include
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([init/3, handle/2, handle/6, terminate/2]).

%%%===================================================================
%% @doc HTTP CallBacks
%%%===================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.

terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% @doc HTTP Handler
%%%===================================================================
handle(Req, State) ->
    {PathList, _} = cowboy_http_req:path(Req),
    {Method, _} = cowboy_http_req:method(Req),

    case PathList of
        [Controller, Action | UrlParams] ->
            handle(Method, Controller, Action, UrlParams, Req, State);
        [Controller] ->
            handle(Method, Controller, <<"index.html">>, [], Req, State);
        [] ->
            handle('GET', <<"shared">>, <<"index.html">>, [], Req, State)
    end.

%%%==================================================================
%%% HTTP Handler
%%%===================================================================

handle('GET', <<"shared">>, FileName, PathList, Req, State) ->
    PathList1 = lists:map(fun(Elem) -> binary_to_list(Elem) end, PathList),
    BasePath = contents_server:shared_dir() ++ "/" ++ binary_to_list(FileName),
    FilePath = contents_server:get_file_path(BasePath, PathList1),

    Req2 = case ets:lookup(shared, FilePath) of
               [] -> 
                   reply(404, Req);
               [{FilePath, Bin}] ->
                   reply(200, [], Bin, Req)
           end,

    {ok, Req2, State};

handle(_, _, _, _, Req, State) ->
    Req2 = reply(404, Req),
    {ok, Req2, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reply(Status, Headers, Body, Req) ->
    {ok, Req2} = cowboy_http_req:reply(Status, Headers, Body, Req),
    Req2.

reply(Status, Req) ->
    {ok, Req2} = cowboy_http_req:reply(Status, Req),
    Req2.

