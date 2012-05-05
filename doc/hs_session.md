

#Module hs_session#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#abandon-1">abandon/1</a></td><td>delete session.</td></tr><tr><td valign="top"><a href="#check_loggedin-2">check_loggedin/2</a></td><td>check already loggedin user?.</td></tr><tr><td valign="top"><a href="#check_loggedin_with_req-1">check_loggedin_with_req/1</a></td><td></td></tr><tr><td valign="top"><a href="#create-1">create/1</a></td><td>ceate new session, and return session key.</td></tr><tr><td valign="top"><a href="#del_value-2">del_value/2</a></td><td>delete session value with key.</td></tr><tr><td valign="top"><a href="#get_session_key_with_req-1">get_session_key_with_req/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_usr-1">get_usr/1</a></td><td>get usr record.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>get session value from key.</td></tr><tr><td valign="top"><a href="#set_value-3">set_value/3</a></td><td>set session value with key.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="abandon-1"></a>

###abandon/1##




<pre>abandon(SessionKey) -&gt; ok</pre>
<ul class="definitions"><li><pre>SessionKey = binary()</pre></li></ul>



delete session.<a name="check_loggedin-2"></a>

###check_loggedin/2##




<pre>check_loggedin(UsrId, SessionKey) -&gt; true | false</pre>
<ul class="definitions"><li><pre>UsrId = binary() | integer()</pre></li><li><pre>SessionKey = binary()</pre></li></ul>



check already loggedin user?<a name="check_loggedin_with_req-1"></a>

###check_loggedin_with_req/1##




<pre>check_loggedin_with_req(Req) -&gt; true | false</pre>
<ul class="definitions"><li><pre>Req = [tuple()]</pre></li></ul>

<a name="create-1"></a>

###create/1##




<pre>create(Usr) -&gt; {ok, SessionKey} | {error, already_exist} | {error, Else}</pre>
<ul class="definitions"><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>SessionKey = binary()</pre></li><li><pre>Else = tuple()</pre></li></ul>



ceate new session, and return session key.<a name="del_value-2"></a>

###del_value/2##




<pre>del_value(SessionKey, Key) -&gt; ok</pre>
<ul class="definitions"><li><pre>SessionKey = binary()</pre></li><li><pre>Key = string() | binary() | integer()</pre></li></ul>



delete session value with key.<a name="get_session_key_with_req-1"></a>

###get_session_key_with_req/1##




<pre>get_session_key_with_req(Req) -&gt; SessionKey</pre>
<ul class="definitions"><li><pre>Req = [tuple()]</pre></li><li><pre>SessionKey = binary()</pre></li></ul>

<a name="get_usr-1"></a>

###get_usr/1##




<pre>get_usr(SessionKey) -&gt; Usr | undefined</pre>
<ul class="definitions"><li><pre>SessionKey = binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>



get usr record.<a name="get_value-2"></a>

###get_value/2##




<pre>get_value(SessionKey, Key) -&gt; {ok, Val} | {error, not_found}</pre>
<ul class="definitions"><li><pre>SessionKey = binary()</pre></li><li><pre>Key = string() | binary() | integer()</pre></li><li><pre>Val = binary()</pre></li></ul>



get session value from key.<a name="set_value-3"></a>

###set_value/3##




<pre>set_value(SessionKey, Key, Val) -&gt; ok</pre>
<ul class="definitions"><li><pre>SessionKey = binary()</pre></li><li><pre>Key = string() | binary() | integer()</pre></li><li><pre>Val = binary() | string() | integer()</pre></li></ul>



set session value with key.<a name="start_link-0"></a>

###start_link/0##




<pre>start_link() -&gt; {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the server
