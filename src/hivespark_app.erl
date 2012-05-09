-module(hivespark_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = hivespark_sup:start_link(),
    unlink(Pid),
    {ok, Pid}.

stop(_State) ->
    ok.
