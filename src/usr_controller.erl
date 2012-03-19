%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created :  9 Mar 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(usr_controller).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([show/4, image/4, save_image/4]).

-define(ICON_DIR, "/Users/shin/var/hivespark_images/").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

show(ParamList, _Req, State, _SessionKey) ->
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
        {ok, Reply} -> {200, jiffy:encode(Reply), State};
        {not_found, ErrorReply} -> {404, jiffy:encode(ErrorReply), State}
    end.

image(ParamList, _Req, State, _SessionKey) ->
    FName = proplists:get_value(<<"fname">>, ParamList),
    Path = lists:flatten([?ICON_DIR, binary_to_list(FName)]),

    case file:read_file(Path) of
        {ok, Binary} -> {200, Binary, State};
        {error, enoent} -> {404, "image not found", State}
    end.

save_image(_ParamList, Req, State, SessionKey) ->
    Usr = hs_session:get_usr(SessionKey),
    {Results, _} = hs_util:acc_multipart(Req, []),
    
    Result = case hs_util:get_multi_data(Results) of
                 {error, not_found} -> error;
                 {ok, Data, Type} ->
                     FName = binary_to_list(Usr#usr.name),
                     Ext = hs_util:ext_part(Type),
                     Path = lists:flatten([?ICON_DIR, FName, Ext]),
                     ok = file:write_file(Path, Data),

                     {ok, BaseUrl} = application:get_env(?APP, base_url),
                     IconUrlStr = lists:flatten([BaseUrl, "/usr/image?fname=", 
                                                 FName, Ext]),
                     IconUrl = list_to_binary(IconUrlStr),
                     {ok, _} = hs_usr:update(Usr#usr{icon_url = IconUrl}),
                     ok
             end,

    Reply = case Result of
                ok -> {[{result, <<"success">>}]};
                _ -> {[{result, <<"failure">>}]}
            end,

    {200, jiffy:encode(Reply), State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

