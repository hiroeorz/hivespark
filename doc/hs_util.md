

#Module hs_util#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).
<a name="types"></a>

##Data Types##




###<a name="type-float_second_datetime">float_second_datetime()</a>##



<pre>float_second_datetime() = {<a href="calendar.md#type-date">calendar:date()</a>, {<a href="calendar.md#type-hour">calendar:hour()</a>, <a href="calendar.md#type-minute">calendar:minute()</a>, float()}}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#acc_multipart-2">acc_multipart/2</a></td><td>
"multipart form-data"リクエストからヘッダーとデータの組を取り出す。.</td></tr><tr><td valign="top"><a href="#create_datetime_string-1">create_datetime_string/1</a></td><td>
主にJSONに格納する為の日時を表す文字列を作り出す。.</td></tr><tr><td valign="top"><a href="#ext_part-1">ext_part/1</a></td><td>
引数で与えられたMineTypeに対応する拡張子を返す。.</td></tr><tr><td valign="top"><a href="#ext_type-1">ext_type/1</a></td><td>
引数で与えられた拡張しに対応するMineTypeを返す。.</td></tr><tr><td valign="top"><a href="#forbidden-1">forbidden/1</a></td><td>
HTTPステータスコード403でアクセス権が無い事を示すレスポンスを返します。.</td></tr><tr><td valign="top"><a href="#forbidden-2">forbidden/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_multi_data-1">get_multi_data/1</a></td><td>
"multipart form-data" リクエストからファイルデータを取り出す。.</td></tr><tr><td valign="top"><a href="#get_param_data-1">get_param_data/1</a></td><td>
"multipart form-data" リクエストからパラメータのリストを取り出す。.</td></tr><tr><td valign="top"><a href="#get_request_params-1">get_request_params/1</a></td><td>
リクエストパラメータを取り出してtupleのlistとして返します。.</td></tr><tr><td valign="top"><a href="#not_authenticated-1">not_authenticated/1</a></td><td>
HTTPステータスコード401でユーザ認証が必要だと伝える為のレスポンスを返します。.</td></tr><tr><td valign="top"><a href="#not_authenticated-2">not_authenticated/2</a></td><td></td></tr><tr><td valign="top"><a href="#not_authenticated-3">not_authenticated/3</a></td><td></td></tr><tr><td valign="top"><a href="#not_found-1">not_found/1</a></td><td>
HTTPステータスコード404で"Not Found"のレスポンスを返します。.</td></tr><tr><td valign="top"><a href="#not_found-2">not_found/2</a></td><td></td></tr><tr><td valign="top"><a href="#ok-2">ok/2</a></td><td>
HTTPステータスコード200でHTTPレスポンスを返します。.</td></tr><tr><td valign="top"><a href="#ok-3">ok/3</a></td><td></td></tr><tr><td valign="top"><a href="#pgdaatetime_to_seconds-1">pgdaatetime_to_seconds/1</a></td><td></td></tr><tr><td valign="top"><a href="#priv_dir-0">priv_dir/0</a></td><td>
privディレクトリへのパスを返す。.</td></tr><tr><td valign="top"><a href="#redirect_to-2">redirect_to/2</a></td><td>
HTTPステータスコード307でHTTPリダイレクト要求のレスポンスを返します。.</td></tr><tr><td valign="top"><a href="#redirect_to-3">redirect_to/3</a></td><td></td></tr><tr><td valign="top"><a href="#reply-2">reply/2</a></td><td></td></tr><tr><td valign="top"><a href="#reply-4">reply/4</a></td><td></td></tr><tr><td valign="top"><a href="#view-2">view/2</a></td><td>
HTMLファイルを読み込んでクライアントに返します。.</td></tr><tr><td valign="top"><a href="#view-3">view/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="acc_multipart-2"></a>

###acc_multipart/2##




`acc_multipart(Req, Acc) -> any()`




"multipart form-data"リクエストからヘッダーとデータの組を取り出す。<a name="create_datetime_string-1"></a>

###create_datetime_string/1##




<pre>create_datetime_string(Seconds) -&gt; DateTimeString</pre>
<ul class="definitions"><li><pre>Seconds = non_neg_integer()</pre></li><li><pre>DateTimeString = binary()</pre></li></ul>




主にJSONに格納する為の日時を表す文字列を作り出す。<a name="ext_part-1"></a>

###ext_part/1##




<pre>ext_part(MineType) -&gt; ExtPart</pre>
<ul class="definitions"><li><pre>MineType = binary()</pre></li><li><pre>ExtPart = string()</pre></li></ul>




引数で与えられたMineTypeに対応する拡張子を返す。<a name="ext_type-1"></a>

###ext_type/1##




<pre>ext_type(ExtPart) -&gt; MineType</pre>
<ul class="definitions"><li><pre>MineType = binary()</pre></li><li><pre>ExtPart = string()</pre></li></ul>




引数で与えられた拡張しに対応するMineTypeを返す。<a name="forbidden-1"></a>

###forbidden/1##




<pre>forbidden(State) -&gt; {403, [], Body, State}</pre>
<ul class="definitions"><li><pre>State = <a href="cowboy_http.md#type-state">cowboy_http:state()</a></pre></li><li><pre>Body = binary()</pre></li></ul>




HTTPステータスコード403でアクセス権が無い事を示すレスポンスを返します。<a name="forbidden-2"></a>

###forbidden/2##




<pre>forbidden(Body, State) -&gt; {403, [], Body, State}</pre>
<ul class="definitions"><li><pre>State = <a href="cowboy_http.md#type-state">cowboy_http:state()</a></pre></li><li><pre>Body = binary()</pre></li></ul>

<a name="get_multi_data-1"></a>

###get_multi_data/1##




<pre>get_multi_data(MultiParts) -&gt; {ok, Data, Type} | {error, not_found}</pre>
<ul class="definitions"><li><pre>MultiParts = [tuple()]</pre></li><li><pre>Data = binary()</pre></li><li><pre>Type = binary()</pre></li></ul>




"multipart form-data" リクエストからファイルデータを取り出す。<a name="get_param_data-1"></a>

###get_param_data/1##




<pre>get_param_data(MultiParts) -&gt; Params</pre>
<ul class="definitions"><li><pre>MultiParts = [tuple()]</pre></li><li><pre>Params = [tuple()]</pre></li></ul>




"multipart form-data" リクエストからパラメータのリストを取り出す。<a name="get_request_params-1"></a>

###get_request_params/1##




<pre>get_request_params(Req) -&gt; ParamList</pre>
<ul class="definitions"><li><pre>Req = any()</pre></li><li><pre>ParamList = [tuple()]</pre></li></ul>




リクエストパラメータを取り出してtupleのlistとして返します。<a name="not_authenticated-1"></a>

###not_authenticated/1##




<pre>not_authenticated(State) -&gt; {401, [], Body, State}</pre>
<ul class="definitions"><li><pre>State = <a href="cowboy_http.md#type-state">cowboy_http:state()</a></pre></li><li><pre>Body = binary()</pre></li></ul>




HTTPステータスコード401でユーザ認証が必要だと伝える為のレスポンスを返します。<a name="not_authenticated-2"></a>

###not_authenticated/2##




<pre>not_authenticated(Body, State) -&gt; {401, [], Body, State}</pre>
<ul class="definitions"><li><pre>State = <a href="cowboy_http.md#type-state">cowboy_http:state()</a></pre></li><li><pre>Body = binary()</pre></li></ul>

<a name="not_authenticated-3"></a>

###not_authenticated/3##




<pre>not_authenticated(Header, Body, State) -&gt; {401, Header, Body, State}</pre>
<ul class="definitions"><li><pre>State = <a href="cowboy_http.md#type-state">cowboy_http:state()</a></pre></li><li><pre>Header = [tuple()]</pre></li><li><pre>Body = binary()</pre></li></ul>

<a name="not_found-1"></a>

###not_found/1##




<pre>not_found(State) -&gt; {404, Header, Body, State}</pre>
<ul class="definitions"><li><pre>State = <a href="cowboy_http.md#type-state">cowboy_http:state()</a></pre></li><li><pre>Header = []</pre></li><li><pre>Body = binary()</pre></li></ul>




HTTPステータスコード404で"Not Found"のレスポンスを返します。<a name="not_found-2"></a>

###not_found/2##




`not_found(Body, State) -> any()`

<a name="ok-2"></a>

###ok/2##




<pre>ok(Body, State) -&gt; {200, Header, Body, State}</pre>
<ul class="definitions"><li><pre>Body = binary()</pre></li><li><pre>State = <a href="cowboy_http.md#type-status">cowboy_http:status()</a></pre></li><li><pre>Header = [tuple()]</pre></li></ul>




HTTPステータスコード200でHTTPレスポンスを返します。<a name="ok-3"></a>

###ok/3##




`ok(Header, Body, State) -> any()`

<a name="pgdaatetime_to_seconds-1"></a>

###pgdaatetime_to_seconds/1##




<pre>pgdaatetime_to_seconds(FDateTime) -&gt; Seconds</pre>
<ul class="definitions"><li><pre>FDateTime = <a href="#type-float_second_datetime">float_second_datetime()</a></pre></li><li><pre>Seconds = non_neg_integer()</pre></li></ul>

<a name="priv_dir-0"></a>

###priv_dir/0##




<pre>priv_dir() -&gt; Path</pre>
<ul class="definitions"><li><pre>Path = string()</pre></li></ul>




privディレクトリへのパスを返す。<a name="redirect_to-2"></a>

###redirect_to/2##




<pre>redirect_to(Url, State) -&gt; {307, Header, binary(), State}</pre>
<ul class="definitions"><li><pre>Url = string()</pre></li><li><pre>State = <a href="cowboy_http.md#type-state">cowboy_http:state()</a></pre></li><li><pre>Header = [tuple()]</pre></li></ul>




HTTPステータスコード307でHTTPリダイレクト要求のレスポンスを返します。<a name="redirect_to-3"></a>

###redirect_to/3##




`redirect_to(Url, Header, State) -> any()`

<a name="reply-2"></a>

###reply/2##




`reply(Status, Req) -> any()`

<a name="reply-4"></a>

###reply/4##




`reply(Status, Headers, Body, Req) -> any()`

<a name="view-2"></a>

###view/2##




<pre>view(Filename, State) -&gt; {200, [], Binary, State}</pre>
<ul class="definitions"><li><pre>Filename = string()</pre></li><li><pre>State = integer()</pre></li><li><pre>Binary = binary()</pre></li><li><pre>State = <a href="cowboy_http.md#type-status">cowboy_http:status()</a></pre></li></ul>




HTMLファイルを読み込んでクライアントに返します。<a name="view-3"></a>

###view/3##




<pre>view(Filename, State, StatusCode) -&gt; {StatusCode, [], Binary, State}</pre>
<ul class="definitions"><li><pre>Filename = string()</pre></li><li><pre>State = integer()</pre></li><li><pre>StatusCode = integer()</pre></li><li><pre>Binary = binary()</pre></li><li><pre>State = <a href="cowboy_http.md#type-status">cowboy_http:status()</a></pre></li></ul>

