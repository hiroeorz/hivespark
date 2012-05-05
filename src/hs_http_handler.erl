%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(hs_http_handler).
-behaviour(cowboy_http_handler).

%% Include
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([init/3, handle/2, terminate/2]).

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
    io:format("unknown route"),
    {ok, Req2} = cowboy_http_req:reply(404, Req),
    {ok, Req2, State}.
