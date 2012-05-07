%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created :  5 May 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_router).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([handle/3, check_accessable/2]).

-record(state, {require_login = false :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% #state{}のrequire_login=trueの場合は認証を行い、そうでない場合は認証せずに
%% アクション関数を呼び出す。
%% @end
%%--------------------------------------------------------------------

handle(Module, Req, #state{require_login = true} = State) ->
    {[_, Action], _} = cowboy_http_req:path(Req),
    Args = hs_util:create_args(Req, State),

    {Req2, NewState} = 
        case hs_session:check_loggedin_with_req(Req) of
            true -> 
                {StatusCode, H, Bin, NS} = Module:handle_route(Action, Args),
                {hs_util:reply(StatusCode, H, Bin, Req), NS};
            false ->
                {StatusCode, H, B, NS} = hs_util:redirect_to("/auth/index", 
                                                             [], State),
                {hs_util:reply(StatusCode, H, B, Req), NS}
        end,

    {ok, Req2, NewState};

handle(Module, Req, #state{require_login = false} = State) ->
    ?debugVal(cowboy_http_req:path(Req)),
    {[_, Action], _} = cowboy_http_req:path(Req),
    Args = hs_util:create_args(Req, State),
    {StatusCode, H, Bin, NS} = Module:handle_route(Action, Args),
    {Req2, NewState} = {hs_util:reply(StatusCode, H, Bin, Req), NS},
    {ok, Req2, NewState}.

check_accessable(_Req, #state{require_login = false}) -> true;
check_accessable(Req, #state{require_login = true}) -> 
    hs_session:check_loggedin_with_req(Req).

%%%===================================================================
%%% Internal functions
%%%===================================================================
