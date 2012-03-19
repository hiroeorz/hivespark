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
-export([ext_part/1, ext_type/1, get_multi_data/1, acc_multipart/2,
        create_datetime_string/1]).

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

get_multi_data([]) -> {error, not_found};
get_multi_data([{Headers, Data}| Tail]) ->
    case Headers of
        ?MultiPartDataPattern -> {ok, Data, Type};
        _ -> get_multi_data(Tail)
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

create_datetime_string(DateTime) when is_tuple(DateTime) ->
    {{Y, M, D}, {H, Min, S}} = 
        case DateTime of
            {{Y1, M1, D1}, {H1, Min1, S1}} -> {{Y1, M1, D1}, {H1, Min1, S1}}; 
            {{Y1, M1, D1}, {H1, Min1, S1, _}} -> {{Y1, M1, D1}, {H1, Min1, S1}}
        end,
    
    Y2 = integer_to_list(Y),
    M2 = string:right(integer_to_list(M), 2, $0),
    D2 = string:right(integer_to_list(D), 2, $0),
    H2 = string:right(integer_to_list(H), 2, $0),
    Min2 = string:right(integer_to_list(Min), 2, $0),
    S2 = case S of
             S3 when is_float(S3) ->
                 string:right(integer_to_list(trunc(S3)), 2, $0);
             S3 when is_integer(S3) ->
                 string:right(integer_to_list(S), 2, $0)
         end,

    list_to_binary(
      lists:flatten(
        io_lib:format("~s-~s-~s ~s:~s:~s", [Y2, M2, D2, H2, Min2, S2]))).

%%%===================================================================
%%% Internal functions
%%%===================================================================

