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
-export([start/0, start_http_listener/2]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(postgres_pool),
    application:start(eredis_pool),
    application:start(cowboy),
    application:start(hivespark),
    {ok, Port} = application:get_env(hivespark, port),
    {ok, ListenerCount} = application:get_env(hivespark, listener_count),
    ?debugVal(Port),
    ?debugVal(ListenerCount),
    start_http_listener(Port, ListenerCount).

start_http_listener(Port, ListenerCount) when is_integer(Port) and
                                              is_integer(ListenerCount) ->
    Dispatch = [{'_',[
                      {[<<"shared">>, '...'], cowboy_http_static,
                       [
                        {directory, <<"./priv/www">>},
                        {mimetypes, [
                                     {<<".html">>, [<<"text/html">>]},
                                     {<<".css">>, [<<"text/css">>]},
                                     {<<".js">>, [<<"application/javascript">>]},
                                     {<<".txt">>, [<<"text/plain">>]},
                                     {<<".jpg">>, [<<"image/jpeg">>]},
                                     {<<".gif">>, [<<"image/gif">>]},
                                     {<<".png">>, [<<"image/png">>]}
                                    ]}
                       ]
                      },

                      {[<<"websocket">>], hs_websocket_handler, []},
                      {'_', hs_http_handler, []}
                     ]
                }],

    cowboy:start_listener(http_listener, ListenerCount,
                         cowboy_tcp_transport, [{port, Port}],
                         cowboy_http_protocol, [{dispatch, Dispatch}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
