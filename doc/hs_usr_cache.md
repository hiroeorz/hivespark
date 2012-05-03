

#Module hs_usr_cache#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_team_id_list-2">add_team_id_list/2</a></td><td>set team id list to redis.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>delete usr from database.</td></tr><tr><td valign="top"><a href="#delete_team_id_list-1">delete_team_id_list/1</a></td><td>delete team id list from redis.</td></tr><tr><td valign="top"><a href="#get_team_id_list-1">get_team_id_list/1</a></td><td>get team id list from redis.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>get usr list from user id list.</td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup user by id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup user by name.</td></tr><tr><td valign="top"><a href="#store-1">store/1</a></td><td>store user cache.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_team_id_list-2"></a>

###add_team_id_list/2##




<pre>add_team_id_list(UsrId, TeamIds) -&gt; ok</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li><li><pre>TeamIds = [integer()]</pre></li></ul>



set team id list to redis.<a name="delete-1"></a>

###delete/1##




<pre>delete(UsrId) -&gt; {ok, deleted} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | string() | binary()</pre></li></ul>



delete usr from database.<a name="delete_team_id_list-1"></a>

###delete_team_id_list/1##




<pre>delete_team_id_list(UsrId) -&gt; ok</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li></ul>



delete team id list from redis.<a name="get_team_id_list-1"></a>

###get_team_id_list/1##




<pre>get_team_id_list(UserId) -&gt; TeamIds</pre>
<ul class="definitions"><li><pre>UserId = integer() | binary()</pre></li><li><pre>TeamIds = [#team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}]</pre></li></ul>



get team id list from redis.<a name="list-1"></a>

###list/1##




<pre>list(UsrIdList) -&gt; UsrList</pre>
<ul class="definitions"><li><pre>UsrIdList = [integer() | string() | binary()]</pre></li><li><pre>UsrList = [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()} | undefined]</pre></li></ul>



get usr list from user id list.<a name="lookup_id-1"></a>

###lookup_id/1##




<pre>lookup_id(UsrId) -&gt; {ok, Usr} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | list() | binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>



lookup user by id.<a name="lookup_name-1"></a>

###lookup_name/1##




<pre>lookup_name(Name) -&gt; {ok, Usr} | {error, not_found}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>



lookup user by name.<a name="store-1"></a>

###store/1##




<pre>store(Usr) -&gt; {ok, Usr} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>



store user cache.