%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hivespark).

%% Include
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/0, start_http_listener/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(emysql),
    application:start(eredis_pool),
    application:start(cowboy),
    application:start(hivespark),
    start_http_listener().

start_http_listener() ->
    Dispatch = [{'_',[
                      {[<<"websocket">>], hivespark_websocket_handler, []},
                      {'_', hivespark_http_handler, []}
                     ]
                }],
    cowboy:start_listener(http_listener, 100,
                         cowboy_tcp_transport, [{port, 8080}],
                         cowboy_http_protocol, [{dispatch, Dispatch}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
