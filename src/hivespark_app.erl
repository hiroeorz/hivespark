-module(hivespark_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("starting hivespark..."),
    application:start(postgres_pool),
    application:start(eredis_pool),
    application:start(cowboy),

    {ok, Pid} = hivespark_sup:start_link(),
    %%unlink(Pid),

    hs_usr_cache:clear_all_worker_pid(),
    {ok, Port} = application:get_env(hivespark, port),
    {ok, ListenerCount} = application:get_env(hivespark, listener_count),
    hivespark:start_http_listener(Port, ListenerCount),

    io:format("ok.~n"),
    {ok, Pid}.

stop(_State) ->
    ok.
