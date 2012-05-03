

#Module hs_message_db#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_msg-1">get_msg/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert-1">insert/1</a></td><td></td></tr><tr><td valign="top"><a href="#list_of_team-3">list_of_team/3</a></td><td></td></tr><tr><td valign="top"><a href="#list_of_usr-2">list_of_usr/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_result-3">parse_result/3</a></td><td>sql query result parser.</td></tr><tr><td valign="top"><a href="#q-1">q/1</a></td><td>exec sql query.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_msg-1"></a>

###get_msg/1##




<pre>get_msg(Id) -&gt; {ok, Message} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Id = integer() | binary()</pre></li><li><pre>Message = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="insert-1"></a>

###insert/1##




<pre>insert(Message) -&gt; {ok, Message} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Message = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="list_of_team-3"></a>

###list_of_team/3##




<pre>list_of_team(TeamId, Offset, Count) -&gt; {ok, Messages} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | binary()</pre></li><li><pre>Offset = integer() | binary()</pre></li><li><pre>Count = integer() | binary()</pre></li><li><pre>Messages = [#message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}]</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="list_of_usr-2"></a>

###list_of_usr/2##




<pre>list_of_usr(UsrId, Count) -&gt; {ok, Messages} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li><li><pre>Count = integer() | binary()</pre></li><li><pre>Messages = [#message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}]</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="parse_result-3"></a>

###parse_result/3##




<pre>parse_result(Columns, Records, Results::[]) -&gt; ParsedResults</pre>
<ul class="definitions"><li><pre>Columns = [tuple()]</pre></li><li><pre>Records = [tuple()]</pre></li><li><pre>ParsedResults = [tuple()]</pre></li></ul>



sql query result parser.<a name="q-1"></a>

###q/1##




<pre>q(Sql) -&gt; ok | {ok, list()} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Sql = string()</pre></li><li><pre>Reason = tuple()</pre></li></ul>



exec sql query.<a name="q-2"></a>

###q/2##




<pre>q(Sql, Params) -&gt; ok | {ok, [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Sql = string()</pre></li><li><pre>Params = [any()]</pre></li><li><pre>Reason = tuple()</pre></li></ul>

