%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created :  5 May 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_router).

%% API
-export([handle/3]).

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
    {PathList, _} = cowboy_http_req:path(Req),
    ParamList = hs_util:get_request_params(Req),
    SessionKey = hs_session:get_session_key_with_req(Req),
    [_, Action] = PathList,
    Args = [ParamList, Req, State, SessionKey],

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
    {PathList, _} = cowboy_http_req:path(Req),
    ParamList = hs_util:get_request_params(Req),
    SessionKey = hs_session:get_session_key_with_req(Req),
    [_, Action] = PathList,
    Args = [ParamList, Req, State, SessionKey],
    {StatusCode, H, Bin, NS} = Module:handle_route(Action, Args),
    {Req2, NewState} = {hs_util:reply(StatusCode, H, Bin, Req), NS},
    {ok, Req2, NewState}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
