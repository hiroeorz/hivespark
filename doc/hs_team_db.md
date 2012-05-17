

#Module hs_team_db#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.

Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td>get all team list.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>delete usr from database.</td></tr><tr><td valign="top"><a href="#insert-4">insert/4</a></td><td>insert new team to database.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>get team list from id list.</td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup user by id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup user by name.</td></tr><tr><td valign="top"><a href="#q-1">q/1</a></td><td>exec sql query.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td></td></tr><tr><td valign="top"><a href="#statuses_list-2">statuses_list/2</a></td><td>get team list of given status.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>update user parameter.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="all-0"></a>

###all/0##


<pre>all() -&gt; [TeamList]</pre>
<ul class="definitions"><li><pre>TeamList = [] | [#team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}]</pre></li></ul>

get all team list.<a name="delete-1"></a>

###delete/1##


<pre>delete(TeamId) -&gt; {ok, deleted} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | string() | binary()</pre></li><li><pre>Reason = tuple()</pre></li></ul>

delete usr from database.<a name="insert-4"></a>

###insert/4##


<pre>insert(Name, OwnerId, IconUrl, Description) -&gt; {ok, Team} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = binary()</pre></li><li><pre>OwnerId = integer()</pre></li><li><pre>IconUrl = binary()</pre></li><li><pre>Description = binary()</pre></li><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

insert new team to database.<a name="list-1"></a>

###list/1##


<pre>list(TeamIdList) -&gt; {ok, TeamList}</pre>
<ul class="definitions"><li><pre>TeamIdList = [integer() | string() | binary()]</pre></li><li><pre>TeamList = [] | [#team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}]</pre></li></ul>

get team list from id list.<a name="lookup_id-1"></a>

###lookup_id/1##


<pre>lookup_id(TeamId) -&gt; {ok, Team} | {error, not_found} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | list() | binary()</pre></li><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = tuple()</pre></li></ul>

lookup user by id.<a name="lookup_name-1"></a>

###lookup_name/1##


<pre>lookup_name(Name) -&gt; {ok, Team} | {error, not_found} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = tuple()</pre></li></ul>

lookup user by name.<a name="q-1"></a>

###q/1##


<pre>q(Sql) -&gt; ok | {ok, [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Sql = string()</pre></li><li><pre>Reason = tuple()</pre></li></ul>

exec sql query.<a name="q-2"></a>

###q/2##


<pre>q(Sql, Params) -&gt; ok | {ok, [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Sql = string()</pre></li><li><pre>Params = [any()]</pre></li><li><pre>Reason = tuple()</pre></li></ul>

<a name="statuses_list-2"></a>

###statuses_list/2##


<pre>statuses_list(Lebel, Count) -&gt; [TeamList]</pre>
<ul class="definitions"><li><pre>Lebel = integer()</pre></li><li><pre>Count = integer()</pre></li><li><pre>TeamList = [] | [#team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}]</pre></li></ul>

get team list of given status.<a name="update-1"></a>

###update/1##


<pre>update(Team) -&gt; {ok, UpdatedTeam} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>UpdatedTeam = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

update user parameter.