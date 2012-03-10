%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_util).

%% API
-export([]).
-export([ext_part/1, ext_type/1, get_multi_data/2, acc_multipart/2]).

-define(MultiPartDataPattern, [{<<"Content-Disposition">>, <<"form-data; name=\"fileName\"; filename=", _N/binary>>}, {'Content-Type', Type}]).

%%%===================================================================
%%% API
%%%===================================================================

ext_part(<<"image/png">>) -> ".png";
ext_part(<<"image/jpeg">>) -> ".jpg";
ext_part(<<"image/gif">>) -> ".gif".

ext_type(".png") -> <<"image/png">>;
ext_type(".jpg") -> <<"image/jpeg">>;
ext_type(".gif") -> <<"image/gif">>.

get_multi_data([], _) -> {error, not_found};
get_multi_data([{Headers, Data}| Tail], _Pattern) ->
    case Headers of
        ?MultiPartDataPattern -> {ok, Data, Type};
        _ -> get_multi_data(Tail, _Pattern)
    end.

acc_multipart(Req, Acc) ->
    {Result, Req2} = cowboy_http_req:multipart_data(Req),
    acc_multipart(Req2, Acc, Result).

acc_multipart(Req, Acc, {headers, Headers}) ->
    acc_multipart(Req, [{Headers, []}|Acc]);

acc_multipart(Req, [{Headers, BodyAcc}|Acc], {body, Data}) ->
    acc_multipart(Req, [{Headers, [Data|BodyAcc]}|Acc]);

acc_multipart(Req, [{Headers, BodyAcc}|Acc], end_of_part) ->
    acc_multipart(Req, [{Headers, list_to_binary(lists:reverse(BodyAcc))}|Acc]);

acc_multipart(Req, Acc, eof) ->
    {lists:reverse(Acc), Req}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
