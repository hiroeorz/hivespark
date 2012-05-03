

#Module hs_usr_db#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_team-2">add_team/2</a></td><td>add usr's team relation.</td></tr><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td>user authenticate.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>delete usr from database.</td></tr><tr><td valign="top"><a href="#delete_team-2">delete_team/2</a></td><td>delete usr's team relation.</td></tr><tr><td valign="top"><a href="#get_team_id_list-1">get_team_id_list/1</a></td><td>get usr's team relation.</td></tr><tr><td valign="top"><a href="#insert-6">insert/6</a></td><td>insert new usr to database.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>get usr list from user id list.</td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup user by id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup user by name.</td></tr><tr><td valign="top"><a href="#parse_result-3">parse_result/3</a></td><td>sql query result parser.</td></tr><tr><td valign="top"><a href="#q-1">q/1</a></td><td>exec sql query.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td></td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>update user parameter.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_team-2"></a>

###add_team/2##




<pre>add_team(UsrId, TeamId) -&gt; ok | {error, already_exist} | {error, missing_team_id}</pre>
<ul class="definitions"><li><pre>UsrId = binary() | integer()</pre></li><li><pre>TeamId = binary() | integer()</pre></li></ul>



add usr's team relation.<a name="authenticate-2"></a>

###authenticate/2##




<pre>authenticate(UsernameBin, PasswordBin) -&gt; {ok, UsrId} | failure</pre>
<ul class="definitions"><li><pre>UsernameBin = binary()</pre></li><li><pre>PasswordBin = binary()</pre></li><li><pre>UsrId = integer()</pre></li></ul>



user authenticate.<a name="delete-1"></a>

###delete/1##




<pre>delete(UsrId) -&gt; {ok, deleted} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | string() | binary()</pre></li><li><pre>Reason = tuple()</pre></li></ul>



delete usr from database.<a name="delete_team-2"></a>

###delete_team/2##




<pre>delete_team(UsrId, TeamId) -&gt; ok</pre>
<ul class="definitions"><li><pre>UsrId = binary() | integer()</pre></li><li><pre>TeamId = binary() | integer()</pre></li></ul>



delete usr's team relation.<a name="get_team_id_list-1"></a>

###get_team_id_list/1##




<pre>get_team_id_list(UsrId) -&gt; [#team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}]</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li></ul>



get usr's team relation.<a name="insert-6"></a>

###insert/6##




<pre>insert(Name, LongName, Mail, Password, IconUrl, Description) -&gt; {ok, Usr} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = binary()</pre></li><li><pre>LongName = binary()</pre></li><li><pre>Mail = binary()</pre></li><li><pre>Password = binary()</pre></li><li><pre>IconUrl = binary()</pre></li><li><pre>Description = binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>



insert new usr to database.<a name="list-1"></a>

###list/1##




<pre>list(UsrIdList) -&gt; {ok, UsrList}</pre>
<ul class="definitions"><li><pre>UsrIdList = [integer() | string() | binary()]</pre></li><li><pre>UsrList = [] | [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]</pre></li></ul>



get usr list from user id list.<a name="lookup_id-1"></a>

###lookup_id/1##




<pre>lookup_id(UsrId) -&gt; {ok, Usr} | {error, not_found} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | list() | binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = tuple()</pre></li></ul>



lookup user by id.<a name="lookup_name-1"></a>

###lookup_name/1##




<pre>lookup_name(Name) -&gt; {ok, Usr} | {error, not_found} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = tuple()</pre></li></ul>



lookup user by name.<a name="parse_result-3"></a>

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

<a name="update-1"></a>

###update/1##




<pre>update(Usr) -&gt; {ok, UpdatedUsr} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>UpdatedUsr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>



update user parameter.