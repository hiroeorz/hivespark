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
         get_multi_data/1, get_param_data/1, acc_multipart/2,
         create_datetime_string/1, pgdaatetime_to_seconds/1,
         get_request_params/1, reply/4, reply/2]).

-define(MultiPartDataPattern, [{<<"Content-Disposition">>, <<"form-data; name=\"fileName\"; filename=", _N/binary>>}, {'Content-Type', Type}]).

-define(MultiPartParamPattern, [{<<"Content-Disposition">>,<<"form-data; name=\"", N/binary>>}]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% HTMLファイルを読み込んでクライアントに返します。
%% @end
%%--------------------------------------------------------------------
-spec view(Filename, State) -> {200, [], Binary, State} when
      Filename :: string(),
      State :: integer(),
      Binary :: binary(),
      State :: cowboy_http:status().
view(Filename, State) ->
    view(Filename, State, 200).

-spec view(Filename, State, StatusCode) -> {StatusCode, [], Binary, State} when
      Filename :: string(),
      State :: integer(),
      StatusCode :: integer(),
      Binary :: binary(),
      State :: cowboy_http:status().
view(Filename, State, StatusCode) ->
    Path = hs_util:priv_dir() ++ "/views/" ++ Filename,
    ?debugVal(Path),
    case file:read_file(Path) of
        {ok, Binary} -> {StatusCode, [], Binary, State};
        {error, enoent} -> not_found(State)
    end.

%%--------------------------------------------------------------------
%% @doc
%% HTTPステータスコード200でHTTPレスポンスを返します。
%% @end
%%--------------------------------------------------------------------
-spec ok(Body, State) -> {200, Header, Body, State} when
      Body :: binary(),
      State :: cowboy_http:status(),
      Header :: [tuple()].
ok(Body, State) when is_binary(Body) ->
    {200, [], Body, State}.

ok(Header, Body, State) when is_list(Header) and is_binary(Body) ->
    {200, Header, Body, State}.

%%--------------------------------------------------------------------
%% @doc
%% HTTPステータスコード307でHTTPリダイレクト要求のレスポンスを返します。
%% @end
%%--------------------------------------------------------------------
-spec redirect_to(Url, State) -> {307, Header, binary(), State} when
      Url :: string(),
      State :: cowboy_http:state(),
      Header :: [tuple()].
redirect_to(Url, State) ->
    redirect_to(Url, [], State).

redirect_to(Url, Header, State) when is_list(Header) ->
    {307, [{'Location', Url} | Header], <<"">>, State}.

%%--------------------------------------------------------------------
%% @doc
%% HTTPステータスコード404で"Not Found"のレスポンスを返します。
%% @end
%%--------------------------------------------------------------------
-spec not_found(State) -> {404, Header, Body, State} when
      State :: cowboy_http:state(),
      Header :: [],
      Body :: binary().
not_found(State) ->
    not_found(<<"page not found">>, State).

not_found(Body, State) ->
    {404, [], Body, State}.

%%--------------------------------------------------------------------
%% @doc
%% HTTPステータスコード401でユーザ認証が必要だと伝える為のレスポンスを返します。
%% @end
%%--------------------------------------------------------------------
-spec not_authenticated(State) -> {401, [], Body, State} when
      State :: cowboy_http:state(),
      Body :: binary().
not_authenticated(State) ->
    not_authenticated(<<"">>, State).

-spec not_authenticated(Body, State) -> {401, [], Body, State} when
      State :: cowboy_http:state(),
      Body :: binary().
not_authenticated(Body, State) ->
    not_authenticated([], Body, State).

-spec not_authenticated(Header, Body, State) -> {401, Header, Body, State} when
      State :: cowboy_http:state(),
      Header :: [tuple()],
      Body :: binary().
not_authenticated(Header, Body, State) ->
    {401, Header, Body, State}.

%%--------------------------------------------------------------------
%% @doc
%% HTTPステータスコード403でアクセス権が無い事を示すレスポンスを返します。
%% @end
%%--------------------------------------------------------------------
-spec forbidden(State) -> {403, [], Body, State} when
      State :: cowboy_http:state(),
      Body :: binary().
forbidden(State) ->
    forbidden(<<"forbidden">>, State).

-spec forbidden(Body, State) -> {403, [], Body, State} when
      State :: cowboy_http:state(),
      Body :: binary().
forbidden(Body, State) ->
    {403, [], Body, State}.

%%--------------------------------------------------------------------
%% @doc
%% privディレクトリへのパスを返す。
%% @end
%%--------------------------------------------------------------------
-spec priv_dir() -> Path when
      Path :: string().
priv_dir() ->
    case code:priv_dir(?APP) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.

%%--------------------------------------------------------------------
%% @doc
%% 引数で与えられたMineTypeに対応する拡張子を返す。
%% @end
%%--------------------------------------------------------------------
-spec ext_part(MineType) -> ExtPart when
      MineType :: binary(),
      ExtPart :: string().
ext_part(<<"image/png">>) -> ".png";
ext_part(<<"image/jpeg">>) -> ".jpg";
ext_part(<<"image/gif">>) -> ".gif".

%%--------------------------------------------------------------------
%% @doc
%% 引数で与えられた拡張しに対応するMineTypeを返す。
%% @end
%%--------------------------------------------------------------------
-spec ext_type(ExtPart) -> MineType when
      MineType :: binary(),
      ExtPart :: string().
ext_type(".png") -> <<"image/png">>;
ext_type(".jpg") -> <<"image/jpeg">>;
ext_type(".gif") -> <<"image/gif">>.

%%--------------------------------------------------------------------
%% @doc
%% "multipart form-data" リクエストからファイルデータを取り出す。
%% @end
%%--------------------------------------------------------------------
-spec get_multi_data(MultiParts) -> {ok, Data, Type} | {error, not_found} when
      MultiParts :: [tuple()],
      Data :: binary(),
      Type :: binary().
get_multi_data([]) -> {error, not_found};
get_multi_data([{Headers, Data}| Tail]) ->
    case Headers of
        ?MultiPartDataPattern -> {ok, Data, Type};
        _ -> get_multi_data(Tail)
    end.

%%--------------------------------------------------------------------
%% @doc
%% "multipart form-data" リクエストからパラメータのリストを取り出す。
%% @end
%%--------------------------------------------------------------------
-spec get_param_data(MultiParts) -> Params when
      MultiParts :: [tuple()],
      Params :: [tuple()].
get_param_data(MultiParts) -> get_param_data(MultiParts, []).

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

%%--------------------------------------------------------------------
%% @doc
%% "multipart form-data"リクエストからヘッダーとデータの組を取り出す。
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% 主にJSONに格納する為の日時を表す文字列を作り出す。
%% @end
%%--------------------------------------------------------------------
-spec create_datetime_string(Seconds) -> DateTimeString when
      Seconds :: non_neg_integer(),
      DateTimeString :: binary().
create_datetime_string(Seconds) when is_integer(Seconds) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = 
        calendar:gregorian_seconds_to_datetime(Seconds),

    list_to_binary(
        io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                      [Year, Month, Day, Hour, Min, Sec])).

-type float_second_datetime() :: {calendar:date(), {calendar:hour(), calendar:minute(), float()}}.

-spec pgdaatetime_to_seconds(FDateTime) -> Seconds when
      FDateTime :: float_second_datetime(),
      Seconds :: non_neg_integer().
pgdaatetime_to_seconds({{Year, Month, Day}, {Hour, Min, Second}}) 
  when is_float(Second)->
    DateTime = {{Year, Month, Day}, {Hour, Min, round(Second)}},
    calendar:datetime_to_gregorian_seconds(DateTime).

%%--------------------------------------------------------------------
%% @doc
%% リクエストパラメータを取り出してtupleのlistとして返します。
%% @end
%%--------------------------------------------------------------------
-spec get_request_params(Req) -> ParamList when
      Req :: any(),
      ParamList :: [tuple()].
get_request_params(Req) ->
    {Method, _} = cowboy_http_req:method(Req),

    case Method of
        'GET' ->
            {ParamList1, _} = cowboy_http_req:qs_vals(Req),
            ParamList1;
        'POST' ->
            case cowboy_http_req:parse_header('Content-Type', Req) of
                {{<<"multipart">>, <<"form-data">>, _}, _} -> [];
                _Else ->
                    case cowboy_http_req:body_qs(Req) of
                        {ParamList2, _} -> ParamList2;
                        _ -> []
                    end
            end
    end.

                        
reply(Status, Headers, Body, Req) ->
    {ok, Req2} = cowboy_http_req:reply(Status, Headers, Body, Req),
    Req2.

reply(Status, Req) ->
    {ok, Req2} = cowboy_http_req:reply(Status, Req),
    Req2.

%%%===================================================================
%%% Internal functions
%%%===================================================================

