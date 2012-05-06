%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created :  5 May 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(team_rest_handler).

-include_lib("eunit/include/eunit.hrl").
-include("deps/cowboy/include/http.hrl").

%% API
-export([init/3, rest_init/2, allowed_methods/2, 
         content_types_provided/2, content_types_accepted/2,
         post_is_create/2, create_path/2, 
         delete_resource/2]).

-export([get_text_html/2, from_html/2, from_json/2]).

-record(state, {require_login = false :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, Opts) ->
    case lists:member(require_login, Opts) of
        true ->  {ok, Req, #state{require_login = true}};
        false -> {ok, Req, #state{require_login = false}}
    end.

allowed_methods(Req, State) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"html">>, []}, get_text_html}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"text">>, <<"html">>, []}, from_text},
      {{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

%%===================================================================
%% @doc
%% チームの新規作成
%% @end
%%===================================================================
-spec post_is_create(Req, State) -> {boolean(), Req, State} when
      Req :: #http_req{},
      State :: #state{}.
 post_is_create(Req, State) ->
    {true, Req, State}.

-spec create_path(Req, State) -> {Location | halt, Req, State} when
      Req :: #http_req{},
      State :: #state{},      
      Location :: binary().
create_path(Req, State) ->
    case hs_router:check_accessable(Req, State) of
        false -> {false, Req, State};
        true -> 
            {Json} = jiffy:decode(Req#http_req.buffer),
            [ParamList1 | Tail] = hs_util:create_args(Req, State),

            case team_handler:create([ParamList1 ++ Json | Tail]) of
                {200, Headers, _, _} ->
                    ReqHeaders1 = Req#http_req.headers,
                    ReqHeaders2 = lists:keymerge(1, ReqHeaders1, 
                                    [{'Content-Type', <<"application/json">>}]),
                    Location = proplists:get_value(<<"Location">>, Headers),
                    {Location, Req#http_req{headers = ReqHeaders2}, State};
                _ -> {halt, Req, State}
            end
    end.

%%===================================================================
%% @doc
%% チームの削除
%% @end
%%===================================================================
-spec delete_resource(Req, State) -> {true | false, Req, State} when
      Req :: #http_req{},
      State :: #state{}.
delete_resource(Req, State) ->
    case hs_router:check_accessable(Req, State) of
        false -> {false, Req, State};
        true ->
            [TeamId] = Req#http_req.path_info,
            [ParamList1 | Tail] = hs_util:create_args(Req, State),
            ParamList2 = [{<<"team_id">>, TeamId} | ParamList1],

            case team_handler:delete([ParamList2 | Tail]) of
                {200, _, _, _} -> {true, Req, State};
                _ -> {false, Req, State}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_text_html(Req, State) -> {Data | false, Req, State} when
      Req :: #http_req{},
      State :: #state{},
      Data :: binary().      
get_text_html(Req, State) ->
    Args = hs_util:create_args(Req, State),

    case Req#http_req.path_info of
        [] -> 
            case team_handler:all(Args) of
                {200, _, Data, _} -> {Data, Req, State};
                _ -> {false, Req, State}
            end;
        [TeamId] ->
            [ParamList1 | Tail] = Args,
            Args1 = [[{<<"team_id">>, TeamId} | ParamList1] | Tail],

            case team_handler:info(Args1) of
                {200, _, Data, _} -> {Data, Req, State};
                _ -> {false, Req, State}
            end
    end.
 
-spec from_html(Req, State) -> {true | false, Req, State} when
      Req :: #http_req{},
      State :: #state{}.
from_html(Req, State) -> {true, Req, State}.

-spec from_json(Req, State) -> {true | false, Req, State} when
      Req :: #http_req{},
      State :: #state{}.
from_json(Req, State) -> {true, Req, State}.
