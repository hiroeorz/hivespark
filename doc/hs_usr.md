

#Module hs_usr#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_message-2">add_message/2</a></td><td>add message to users timeline.</td></tr><tr><td valign="top"><a href="#add_team-2">add_team/2</a></td><td></td></tr><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td></td></tr><tr><td valign="top"><a href="#checkin_to_team-2">checkin_to_team/2</a></td><td></td></tr><tr><td valign="top"><a href="#create-6">create/6</a></td><td>create new user.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>delete user.</td></tr><tr><td valign="top"><a href="#delete_team-2">delete_team/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_checkin_team-1">get_checkin_team/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_messages-2">get_messages/2</a></td><td>get messages for users timeline.</td></tr><tr><td valign="top"><a href="#get_messages-3">get_messages/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_teams-1">get_teams/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup user by id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup user by name.</td></tr><tr><td valign="top"><a href="#to_tuple-1">to_tuple/1</a></td><td>parse usr for json object.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>update user.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_message-2"></a>

###add_message/2##




<pre>add_message(UsrId, MsgId) -&gt; ok</pre>
<ul class="definitions"><li><pre>UsrId = integer()</pre></li><li><pre>MsgId = binary()</pre></li></ul>



add message to users timeline.<a name="add_team-2"></a>

###add_team/2##




<pre>add_team(UsrId, TeamId) -&gt; ok</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li><li><pre>TeamId = integer() | binary()</pre></li></ul>

<a name="authenticate-2"></a>

###authenticate/2##




<pre>authenticate(UsernameBin, PasswordBin) -&gt; {ok, UsrId} | failure</pre>
<ul class="definitions"><li><pre>UsernameBin = binary()</pre></li><li><pre>PasswordBin = binary()</pre></li><li><pre>UsrId = integer()</pre></li></ul>

<a name="checkin_to_team-2"></a>

###checkin_to_team/2##




<pre>checkin_to_team(UsrId, TeamId) -&gt; ok</pre>
<ul class="definitions"><li><pre>UsrId = integer()</pre></li><li><pre>TeamId = integer()</pre></li></ul>

<a name="create-6"></a>

###create/6##




<pre>create(Name, LongName, Mail, Password, IconUrl, Description) -&gt; {ok, Usr} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = binary()</pre></li><li><pre>LongName = binary()</pre></li><li><pre>Mail = binary()</pre></li><li><pre>Password = binary()</pre></li><li><pre>IconUrl = binary()</pre></li><li><pre>Description = binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>



create new user.<a name="delete-1"></a>

###delete/1##




<pre>delete(UsrId) -&gt; {ok, deleted} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | string() | binary()</pre></li></ul>



delete user.<a name="delete_team-2"></a>

###delete_team/2##




<pre>delete_team(UsrId, TeamId) -&gt; ok</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li><li><pre>TeamId = integer() | binary()</pre></li></ul>

<a name="get_checkin_team-1"></a>

###get_checkin_team/1##




<pre>get_checkin_team(UsrId) -&gt; {ok, Team} | {error, not_found} | {error, not_checkin_user}</pre>
<ul class="definitions"><li><pre>UsrId = integer()</pre></li><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>

<a name="get_messages-2"></a>

###get_messages/2##




<pre>get_messages(UsrId, Count) -&gt; {ok, MessageList} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer()</pre></li><li><pre>Count = integer()</pre></li><li><pre>MessageList = [#message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}] | []</pre></li></ul>



get messages for users timeline.<a name="get_messages-3"></a>

###get_messages/3##




<pre>get_messages(UsrId, Offset, Count) -&gt; {ok, MessageList} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer()</pre></li><li><pre>Offset = integer()</pre></li><li><pre>Count = integer()</pre></li><li><pre>MessageList = [#message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}] | []</pre></li></ul>

<a name="get_teams-1"></a>

###get_teams/1##




<pre>get_teams(UsrId) -&gt; {ok, [#team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="lookup_id-1"></a>

###lookup_id/1##




<pre>lookup_id(UsrId) -&gt; {ok, Usr} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | list() | binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>



lookup user by id.<a name="lookup_name-1"></a>

###lookup_name/1##




<pre>lookup_name(Name) -&gt; {ok, Usr} | {error, not_found}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>



lookup user by name.<a name="to_tuple-1"></a>

###to_tuple/1##




<pre>to_tuple(Usr) -&gt; TupleUsr</pre>
<ul class="definitions"><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()} | integer()</pre></li><li><pre>TupleUsr = tuple()</pre></li></ul>



parse usr for json object.<a name="update-1"></a>

###update/1##




<pre>update(Usr) -&gt; {ok, NewUsr} | {error, not_found}</pre>
<ul class="definitions"><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>NewUsr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>



update user.