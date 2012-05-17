

#Module hs_team#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.

Copyright (c) (C) 2012, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_article-1">add_article/1</a></td><td></td></tr><tr><td valign="top"><a href="#add_message-1">add_message/1</a></td><td>add message to users timeline.</td></tr><tr><td valign="top"><a href="#checkin-3">checkin/3</a></td><td>checkin usr in team room.</td></tr><tr><td valign="top"><a href="#checkin_members-1">checkin_members/1</a></td><td>all checkin members in team room.</td></tr><tr><td valign="top"><a href="#checkout-3">checkout/3</a></td><td>checkout usr from team room.</td></tr><tr><td valign="top"><a href="#create-4">create/4</a></td><td>create new team.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>delete team.</td></tr><tr><td valign="top"><a href="#get_articles-4">get_articles/4</a></td><td>add message to users timeline.</td></tr><tr><td valign="top"><a href="#get_latest_message-1">get_latest_message/1</a></td><td>get latest message.</td></tr><tr><td valign="top"><a href="#get_members-1">get_members/1</a></td><td>
add message to users timeline.</td></tr><tr><td valign="top"><a href="#get_members_pids-1">get_members_pids/1</a></td><td>
get team members websocket pid list.</td></tr><tr><td valign="top"><a href="#get_messages-3">get_messages/3</a></td><td>add message to team timeline.</td></tr><tr><td valign="top"><a href="#get_messages_by_since_id-2">get_messages_by_since_id/2</a></td><td>add message to team timeline.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_member-2">is_member/2</a></td><td>check added Usr is member of Team.</td></tr><tr><td valign="top"><a href="#is_owner-2">is_owner/2</a></td><td>check added Usr is owner of Team.</td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup team by id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup team by name.</td></tr><tr><td valign="top"><a href="#start_child-1">start_child/1</a></td><td>
Starts the server under the supervisor.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#statuses_list-2">statuses_list/2</a></td><td>get team list of given status.</td></tr><tr><td valign="top"><a href="#to_tuple-1">to_tuple/1</a></td><td>parse team for json object.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>update team.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_article-1"></a>

###add_article/1##


<pre>add_article(Art) -&gt; {ok, Art2} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Art = #article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Art2 = #article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="add_message-1"></a>

###add_message/1##


<pre>add_message(Msg) -&gt; {ok, Msg2} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Msg = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Msg2 = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

add message to users timeline.<a name="checkin-3"></a>

###checkin/3##


<pre>checkin(SessionKey, TeamId, UsrId) -&gt; {ok, Members} | {error, Reason}</pre>
<ul class="definitions"><li><pre>SessionKey = binary()</pre></li><li><pre>TeamId = integer() | binary()</pre></li><li><pre>UsrId = integer() | binary()</pre></li><li><pre>Members = [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]</pre></li><li><pre>Reason = atom()</pre></li></ul>

checkin usr in team room.<a name="checkin_members-1"></a>

###checkin_members/1##


<pre>checkin_members(TeamId) -&gt; Members</pre>
<ul class="definitions"><li><pre>TeamId = integer() | binary()</pre></li><li><pre>Members = [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]</pre></li></ul>

all checkin members in team room.<a name="checkout-3"></a>

###checkout/3##


<pre>checkout(SessionKey, TeamId, UsrId) -&gt; {ok, Members} | {error, Reason}</pre>
<ul class="definitions"><li><pre>SessionKey = binary()</pre></li><li><pre>TeamId = integer() | binary()</pre></li><li><pre>UsrId = integer() | binary()</pre></li><li><pre>Members = [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]</pre></li><li><pre>Reason = atom()</pre></li></ul>

checkout usr from team room.<a name="create-4"></a>

###create/4##


<pre>create(Name, OwnerId, IconUrl, Description) -&gt; {ok, Team} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = binary()</pre></li><li><pre>OwnerId = integer() | binary()</pre></li><li><pre>IconUrl = binary()</pre></li><li><pre>Description = binary()</pre></li><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

create new team.<a name="delete-1"></a>

###delete/1##


<pre>delete(TeamId) -&gt; {ok, deleted} | {error, not_found}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | string() | binary()</pre></li></ul>

delete team.<a name="get_articles-4"></a>

###get_articles/4##


<pre>get_articles(TeamId, Offset, Count, Status) -&gt; {ok, ArticleList} | {error, not_found}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | binary()</pre></li><li><pre>Offset = integer()</pre></li><li><pre>Count = integer()</pre></li><li><pre>Status = integer()</pre></li><li><pre>ArticleList = [#article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}] | []</pre></li></ul>

add message to users timeline.<a name="get_latest_message-1"></a>

###get_latest_message/1##


<pre>get_latest_message(TeamId) -&gt; {ok, Message} | {error, not_found}</pre>
<ul class="definitions"><li><pre>TeamId = binary() | integer()</pre></li><li><pre>Message = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li></ul>

get latest message.<a name="get_members-1"></a>

###get_members/1##


<pre>get_members(TeamId) -&gt; {ok, UsrList} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | binary()</pre></li><li><pre>UsrList = [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]</pre></li><li><pre>Reason = atom()</pre></li></ul>


add message to users timeline.<a name="get_members_pids-1"></a>

###get_members_pids/1##


<pre>get_members_pids(TeamId) -&gt; PidList</pre>
<ul class="definitions"><li><pre>TeamId = integer()</pre></li><li><pre>PidList = [pid()] | []</pre></li></ul>


get team members websocket pid list.<a name="get_messages-3"></a>

###get_messages/3##


<pre>get_messages(TeamId, Offset, Count) -&gt; {ok, MessageList} | {error, not_found}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | binary()</pre></li><li><pre>Offset = integer()</pre></li><li><pre>Count = integer()</pre></li><li><pre>MessageList = [#message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}] | []</pre></li></ul>

add message to team timeline.<a name="get_messages_by_since_id-2"></a>

###get_messages_by_since_id/2##


<pre>get_messages_by_since_id(TeamId, SinceId) -&gt; {ok, MessageList} | {error, not_found}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | binary()</pre></li><li><pre>SinceId = binary()</pre></li><li><pre>MessageList = [#message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}] | []</pre></li></ul>

add message to team timeline.<a name="get_pid-1"></a>

###get_pid/1##


<pre>get_pid(TeamId) -&gt; {ok, pid()} | {error, not_found}</pre>
<ul class="definitions"><li><pre>TeamId = integer()</pre></li></ul>

<a name="is_member-2"></a>

###is_member/2##


<pre>is_member(TeamId, UsrId) -&gt; true | false</pre>
<ul class="definitions"><li><pre>TeamId = integer()</pre></li><li><pre>UsrId = integer()</pre></li></ul>

check added Usr is member of Team.<a name="is_owner-2"></a>

###is_owner/2##


<pre>is_owner(TeamId, UsrId) -&gt; true | false</pre>
<ul class="definitions"><li><pre>TeamId = integer()</pre></li><li><pre>UsrId = integer()</pre></li></ul>

check added Usr is owner of Team.<a name="lookup_id-1"></a>

###lookup_id/1##


<pre>lookup_id(TeamId) -&gt; {ok, Team} | {error, not_found}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | list() | binary()</pre></li><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>

lookup team by id.<a name="lookup_name-1"></a>

###lookup_name/1##


<pre>lookup_name(Name) -&gt; {ok, Team} | {error, not_found}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li></ul>

lookup team by name.<a name="start_child-1"></a>

###start_child/1##


<pre>start_child(TeamId) -&gt; {ok, Pid} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = binary() | integer()</pre></li><li><pre>Pid = pid()</pre></li><li><pre>Reason = atom()</pre></li></ul>


Starts the server under the supervisor.<a name="start_link-1"></a>

###start_link/1##


<pre>start_link(TeamId) -&gt; {ok, Pid} | ignore | {error, Error}</pre>
<ul class="definitions"><li><pre>TeamId = integer()</pre></li><li><pre>Pid = pid()</pre></li><li><pre>Error = atom()</pre></li></ul>


Starts the server<a name="statuses_list-2"></a>

###statuses_list/2##


<pre>statuses_list(Lebel, Count) -&gt; [TeamList]</pre>
<ul class="definitions"><li><pre>Lebel = integer()</pre></li><li><pre>Count = integer()</pre></li><li><pre>TeamList = [] | [#team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}]</pre></li></ul>

get team list of given status.<a name="to_tuple-1"></a>

###to_tuple/1##


<pre>to_tuple(Team) -&gt; TupledTeam</pre>
<ul class="definitions"><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()} | integer()</pre></li><li><pre>TupledTeam = [tuple()]</pre></li></ul>

parse team for json object.<a name="update-1"></a>

###update/1##


<pre>update(Team) -&gt; {ok, Team} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Team = #team{id = undefined | integer(), name = undefined | string(), owner_id = undefined | integer(), icon_url = string(), description = string(), status = integer(), status_description = string(), created_at = undefined | non_neg_integer()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

update team.