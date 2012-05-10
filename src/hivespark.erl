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
    hs_usr_cache:clear_all_worker_pid(),
    {ok, Port} = application:get_env(hivespark, port),
    {ok, ListenerCount} = application:get_env(hivespark, listener_count),
    start_http_listener(Port, ListenerCount).

start_http_listener(Port, ListenerCount) when is_integer(Port) and
                                              is_integer(ListenerCount) ->

    SharedDispatch = {[<<"shared">>, '...'], cowboy_http_static,
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

    Dispatch = 
        [{'_',[SharedDispatch,
               {[<<"websock">>, <<"notification">>], notification_handler, []},

               {[<<"rest">>, <<"team">>, '...'], 
                team_rest_handler, [require_login]},

               {[<<"team">>, '...'], team_handler, [require_login]},
               {[<<"usr">>, '...'], usr_handler, [require_login]},
               {[<<"article">>, '...'], article_handler, [require_login]},
               {[<<"auth">>, '...'], auth_handler, []},
               {[<<"usr_public">>, '...'], usr_public_handler, []},
               {'_', hs_http_handler, []}
              ]
         }],

    cowboy:start_listener(http_listener, ListenerCount,
                          cowboy_tcp_transport, [{port, Port}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
