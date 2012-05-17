

#Module hs_usr_team_db#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.

Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_teams_usrs-1">delete_teams_usrs/1</a></td><td>delete usrs from team_id.</td></tr><tr><td valign="top"><a href="#get_teams_usrs-1">get_teams_usrs/1</a></td><td>get usrs from team_id.</td></tr><tr><td valign="top"><a href="#q-1">q/1</a></td><td>exec sql query.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="delete_teams_usrs-1"></a>

###delete_teams_usrs/1##


<pre>delete_teams_usrs(TeamId) -&gt; {ok, deleted} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = integer()</pre></li><li><pre>Reason = atom()</pre></li></ul>

delete usrs from team_id.<a name="get_teams_usrs-1"></a>

###get_teams_usrs/1##


<pre>get_teams_usrs(TeamId) -&gt; {ok, [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), password_seed = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | non_neg_integer()}]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = integer()</pre></li><li><pre>Reason = atom()</pre></li></ul>

get usrs from team_id.<a name="q-1"></a>

###q/1##


<pre>q(Sql) -&gt; ok | {ok, list()} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Sql = string()</pre></li><li><pre>Reason = tuple()</pre></li></ul>

exec sql query.<a name="q-2"></a>

###q/2##


<pre>q(Sql, Params) -&gt; ok | {ok, [#usr_team{usr_id = undefined | integer(), team_id = undefined | integer(), created_at = undefined | non_neg_integer()}]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Sql = string()</pre></li><li><pre>Params = [any()]</pre></li><li><pre>Reason = tuple()</pre></li></ul>

