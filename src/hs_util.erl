%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_util).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([]).
-export([view/2, view/3, ok/2, ok/3, not_found/1, not_found/2,
         forbidden/1, forbidden/2,
         not_authenticated/1, not_authenticated/2, not_authenticated/3,
         redirect_to/2, redirect_to/3,
         priv_dir/0, ext_part/1, ext_type/1, 
         get_multi_data/1, get_param_data/2, acc_multipart/2,
         create_datetime_string/1]).

-define(MultiPartDataPattern, [{<<"Content-Disposition">>, <<"form-data; name=\"fileName\"; filename=", _N/binary>>}, {'Content-Type', Type}]).

-define(MultiPartParamPattern, [{<<"Content-Disposition">>,<<"form-data; name=\"", N/binary>>}]).

%%%===================================================================
%%% API
%%%===================================================================

view(Filename, State) ->
    view(Filename, State, 200).

view(Filename, State, StatusCode) ->
    Path = hs_util:priv_dir() ++ "/views/" ++ Filename,
    ?debugVal(Path),
    case file:read_file(Path) of
        {ok, Binary} -> {StatusCode, [], Binary, State};
        {error, enoent} -> not_found(State)
    end.

ok(Body, State) when is_binary(Body) ->
    {200, [], Body, State}.

ok(Header, Body, State) when is_list(Header) and is_binary(Body) ->
    {200, Header, Body, State}.

redirect_to(Url, State) ->
    redirect_to(Url, [], State).

redirect_to(Url, Header, State) when is_list(Header) ->
    {307, [{'Location', Url} | Header], <<"">>, State}.

not_found(State) ->
    not_found("page not found", State).

not_found(Body, State) ->
    {404, [], Body, State}.

not_authenticated(State) ->
    not_authenticated(<<"">>, State).

not_authenticated(Body, State) ->
    not_authenticated([], Body, State).

not_authenticated(Header, Body, State) ->
    {401, Header, Body, State}.

forbidden(State) ->
    not_found("forbidden", State).

forbidden(Body, State) ->
    {403, [], Body, State}.

priv_dir() ->
    case code:priv_dir(?APP) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.

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

get_param_data([], Params) -> Params;
get_param_data([{Headers, Data}| Tail], Params) ->
    case Headers of
        ?MultiPartParamPattern -> 
            N2 = list_to_binary(re:replace(binary_to_list(N), "\"", "", 
                                           [{return, list}])),
            get_param_data(Tail, [{N2, Data} | Params]);
        _ -> 
            get_param_data(Tail, Params)
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

