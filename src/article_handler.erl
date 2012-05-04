%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(article_handler).
-behaviour(cowboy_http_handler).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([init/3, handle/2, terminate/2, handle_route/2]).

-record(state, {require_login = false :: boolean()}).

%%%===================================================================
%%% @doc HTTP CallBacks
%%% @end
%%%===================================================================

init({tcp, http}, Req, Options) ->
    case lists:member(require_login, Options) of
        true ->
            {ok, Req, #state{require_login = true}};
        false ->
            {ok, Req, #state{require_login = false}}
    end.

terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% @doc HTTP Handler
%%% @end
%%%===================================================================
handle(Req, State) -> hs_router:handle(?MODULE, Req, State).

handle_route(Action, Args) ->
    case Action of
        <<"create">>           -> create(Args);
        <<"update">>           -> update(Args);
        <<"teams_list">>       -> teams_list(Args);
        <<"status_increment">> -> status_increment(Args);
        <<"status_decrement">> -> status_decrement(Args)
    end.

%%%===================================================================
%%% @doc Request Handle Functions
%%% @end
%%%===================================================================

create([ParamList, _Req, State, SessionKey]) ->
    TeamIdBin = proplists:get_value(<<"team_id">>, ParamList),
    TeamId = list_to_integer(binary_to_list(TeamIdBin)),
    TextBin = proplists:get_value(<<"text">>, ParamList),
    TitleBin = proplists:get_value(<<"title">>, ParamList),

    Type = case proplists:get_value(<<"type">>, ParamList) of
               undefined -> 0;
               T -> list_to_integer(binary_to_list(T))
           end,

    Usr = hs_session:get_usr(SessionKey),
    Art = #article{usr_id = Usr#usr.id, team_id = TeamId, text = TextBin, 
                   title = TitleBin, type = Type, status = 0,
                   progress = 0},

    Reply = case hs_team:add_article(Art) of
                {ok, Article} ->
                    TArt = hs_article_db:to_tuple(Article),
                    {[{<<"result">>, true}, {article, TArt}]};
                {error, Reason} ->
                    {[{<<"result">>, false}, 
                      {<<"reason">>, list_to_binary(atom_to_list(Reason))}]}
            end,    
    hs_util:ok(jiffy:encode(Reply), State).    

update([ParamList, _Req, State, SessionKey]) ->
    Usr = hs_session:get_usr(SessionKey),
    Title = proplists:get_value(<<"title">>, ParamList),
    Text = proplists:get_value(<<"text">>, ParamList),
    ProgressBin = proplists:get_value(<<"progress">>, ParamList),
    Progress = list_to_integer(binary_to_list(ProgressBin)),

    ArticleId = proplists:get_value(<<"article_id">>, ParamList),
    
    case hs_article_db:lookup_id(ArticleId) of
        {error, not_found} -> hs_util:not_found(State);
        {ok, Article} ->
            case hs_team:is_member(Article#article.team_id, Usr#usr.id) of
                false -> hs_util:forbidden(State);
                true ->
                    NewArticle = Article#article{title = Title, text = Text,
                                                 progress = Progress},
                    {ok, NewArticle1} = hs_article_db:update(NewArticle),
                    TArt = hs_article_db:to_tuple(NewArticle1),
                    Reply = {[{<<"result">>, true}, {article, TArt}]},
                    hs_util:ok(jiffy:encode(Reply), State)
            end
    end.

teams_list([ParamList, _Req, State, _SessionKey]) ->
    TeamId = proplists:get_value(<<"team_id">>, ParamList),
    Count = case proplists:get_value(<<"count">>, ParamList) of
                undefined -> 40;
                CountBin -> list_to_integer(binary_to_list(CountBin))
            end,
    Offset = case proplists:get_value(<<"offset">>, ParamList) of
                 undefined -> 0;
                 OffsetBin -> list_to_integer(binary_to_list(OffsetBin))
             end,
    Status = case proplists:get_value(<<"status">>, ParamList) of
                 undefined -> 0;
                 StatusBin-> list_to_integer(binary_to_list(StatusBin))
             end,

    Reply = case hs_team:get_articles(TeamId, Offset, Count, Status) of
                {error, not_found} ->
                    {[{<<"error">>, <<"not_found">>}]};
                {ok, ArticleList} ->
                    lists:map(fun(A) -> hs_article_db:to_tuple(A) end,
                              ArticleList)
            end,
    hs_util:ok(jiffy:encode(Reply), State).

status_increment([ParamList, _Req, State, SessionKey]) ->
    Usr = hs_session:get_usr(SessionKey),
    ArticleId = proplists:get_value(<<"article_id">>, ParamList),

    case hs_article_db:lookup_id(ArticleId) of
        {ok, Article} -> 
            case hs_team:is_member(Article#article.team_id, Usr#usr.id) of
                false -> hs_util:forbidden(State);
                true ->
                    case Article#article.progress of
                        100 -> status_add(1, Usr, Article, State);
                        _ ->
                            case Article#article.status of
                                0 -> status_add(1, Usr, Article, State);
                                1 -> status_add(1, Usr, Article, State);
                                _ ->
                                    Reply = {[{<<"result">>, false},
                                              {article, hs_article_db:to_tuple(Article)}]},
                                    hs_util:ok(jiffy:encode(Reply), State)
                            end
                    end
                end;
        {error, Reason} ->
            Reply = {[{<<"result">>, false}, 
                      {<<"reason">>, list_to_binary(atom_to_list(Reason))}]},
            hs_util:not_found(Reply, State)
    end.

status_decrement([ParamList, _Req, State, SessionKey]) ->
    Usr = hs_session:get_usr(SessionKey),
    ArticleId = proplists:get_value(<<"article_id">>, ParamList),

    case hs_article_db:lookup_id(ArticleId) of
        {ok, Article} -> 
            status_add(-1, Usr, Article, State);
        {error, Reason} ->
            Reply = {[{<<"result">>, false}, 
                      {<<"reason">>, list_to_binary(atom_to_list(Reason))}]},
            hs_util:not_found(Reply, State)
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

status_add(AddValue, Usr, Article, State) ->
    NewStatus = Article#article.status + AddValue,
    NewArticle = Article#article{status = NewStatus, 
                                 usr_id = Usr#usr.id,
                                 progress = 0},

    case hs_article_db:update(NewArticle) of
        {ok, NewArticle1} ->
            Reply = {[{<<"result">>, true},
                      {article, hs_article_db:to_tuple(NewArticle1)}]},
            hs_util:ok(jiffy:encode(Reply), State);
        {error, not_found} ->
            Reply = {[{<<"result">>, false}, 
                      {<<"reason">>, <<"not found">>}]},
            hs_util:not_found(jiffy:encode(Reply), State)
    end.
