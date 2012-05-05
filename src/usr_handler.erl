%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(usr_handler).
-behaviour(cowboy_http_handler).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([init/3, handle/2, terminate/2, handle_route/2]).

-record(state, {require_login = false :: boolean()}).

-define(ICON_DIR, "/usr/local/var/hivespark/images/").

%%%===================================================================
%% @doc HTTP CallBacks
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
%%%===================================================================
handle(Req, State) -> hs_router:handle(?MODULE, Req, State).

handle_route(Action, Args) ->
    case Action of
        <<"edit">>        -> edit(Args);
        <<"upload_icon">> -> upload_icon(Args);
        <<"logout">>      -> logout(Args);
        <<"show_myself">> -> show_myself(Args);
        <<"show">>        -> show(Args);
        <<"update">>      -> update(Args);
        <<"image">>       -> image(Args);
        <<"save_image">>  -> save_image(Args)
    end.
    
%%%===================================================================
%%% Request Handle Functions
%%%===================================================================

edit([_ParamList, _Req, State, _SessionKey]) ->
    hs_util:view("usr_edit.html", State).

upload_icon([_ParamList, _Req, State, _SessionKey]) ->
    hs_util:view("usr_upload_icon.html", State).

logout([ParamList, _Req, State, SessionKey]) ->
    Format = proplists:get_value(<<"format">>, ParamList),
    ok = hs_session:abandon(SessionKey),

    Cookie1 = cowboy_cookies:cookie(<<"session_key">>, <<"">>,
                                    [{path, <<"/">>}]),
    Cookie2 = cowboy_cookies:cookie(<<"usr_id">>, <<"">>,
                                    [{path, <<"/">>}]),

    case Format of
        <<"html">> ->
            hs_util:redirect_to("/auth/index", [Cookie1, Cookie2], State);
        _ ->
            hs_util:ok([Cookie1, Cookie2], jiffy:encode({[{result, true}]}), State)
    end.
    
show_myself([_ParamList, _Req, State, SessionKey]) ->
    Usr = hs_session:get_usr(SessionKey),
    Reply = {[{usr, hs_usr:to_tuple(Usr)}]},
    hs_util:ok(jiffy:encode(Reply), State).

show([ParamList, _Req, State, _SessionKey]) ->
    UsrId = proplists:get_value(<<"usr_id">>, ParamList),
    Name = proplists:get_value(<<"name">>, ParamList),

    Result = case UsrId of
                 undefined ->
                     case hs_usr:lookup_name(Name) of
                         {ok, Usr} -> {ok, {[{usr, hs_usr:to_tuple(Usr)}]}};
                         {error, _Reason} -> 
                             {not_found, {[{error, <<"not_found">>}]}}
                     end;
                 Id ->
                     case hs_usr:lookup_id(Id) of
                         {ok, Usr} -> {ok, {[{usr, hs_usr:to_tuple(Usr)}]}};
                         {error, _Reason} -> 
                             {not_found, {[{error, <<"not_found">>}]}}
                     end
             end,
    
    case Result of
        {ok, Reply} -> hs_util:ok(jiffy:encode(Reply), State);
        {not_found, ErrorReply} -> 
            hs_util:not_found(jiffy:encode(ErrorReply), State)
    end.

update([ParamList, _Req, State, SessionKey]) ->
    Usr = hs_session:get_usr(SessionKey),
    Name = proplists:get_value(<<"name">>, ParamList),
    LongName = proplists:get_value(<<"longname">>, ParamList),
    EMail = proplists:get_value(<<"email">>, ParamList),
    Description = proplists:get_value(<<"description">>, ParamList),
    Format = proplists:get_value(<<"format">>, ParamList),

    Usr1 = Usr#usr{name=Name, longname=LongName, email=EMail,
                   description=Description},
    {ok, Usr2} = hs_usr:update(Usr1),
    Reply = {[{<<"result">>, true}, {usr, hs_usr:to_tuple(Usr2)}]},

    case Format of
        <<"html">> ->
            hs_util:redirect_to("/team/index", State);
        _ ->
            hs_util:ok(jiffy:encode(Reply), State)
    end.


image([ParamList, _Req, State, _SessionKey]) ->
    FName = proplists:get_value(<<"fname">>, ParamList),
    Path = lists:flatten([?ICON_DIR, binary_to_list(FName)]),

    case file:read_file(Path) of
        {ok, Binary} -> hs_util:ok(Binary, State);
        {error, enoent} -> hs_util:not_found("image not found", State)
    end.

save_image([_, Req, State, SessionKey]) ->
    {Results, _} = hs_util:acc_multipart(Req, []),
    ParamList = hs_util:get_param_data(Results),
    Format = proplists:get_value(<<"format">>, ParamList),
    Usr = hs_session:get_usr(SessionKey),
    
    Result = case hs_util:get_multi_data(Results) of
                 {error, not_found} -> error;
                 {ok, Data, Type} ->
                     FName = integer_to_list(Usr#usr.id),
                     Ext = hs_util:ext_part(Type),
                     Path = lists:flatten([?ICON_DIR, FName, Ext]),
                     ok = file:write_file(Path, Data),

                     IconUrlStr = lists:flatten(["/usr/image?fname=", FName, Ext]),
                     IconUrl = list_to_binary(IconUrlStr),
                     {ok, _} = hs_usr:update(Usr#usr{icon_url = IconUrl}),
                     ok
             end,

    Reply = case Result of
                ok -> {[{result, true}]};
                _ -> {[{result, false}]}
            end,

    case Format of
        <<"html">> ->
            hs_util:view("usr_upload_icon.html", State);
        _ ->
            hs_util:ok(jiffy:encode(Reply), State)
    end.

