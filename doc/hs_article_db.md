

#Module hs_article_db#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.

Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#insert-1">insert/1</a></td><td></td></tr><tr><td valign="top"><a href="#list_of_team-4">list_of_team/4</a></td><td></td></tr><tr><td valign="top"><a href="#list_of_usr-2">list_of_usr/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_result-3">parse_result/3</a></td><td>sql query result parser.</td></tr><tr><td valign="top"><a href="#q-1">q/1</a></td><td>exec sql query.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_tuple-1">to_tuple/1</a></td><td>parse usr for json object.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="insert-1"></a>

###insert/1##


<pre>insert(Article) -&gt; {ok, Article} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Article = #article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="list_of_team-4"></a>

###list_of_team/4##


<pre>list_of_team(TeamId, Offset, Count, Status) -&gt; {ok, Articles} | {error, Reason}</pre>
<ul class="definitions"><li><pre>TeamId = integer() | binary()</pre></li><li><pre>Offset = integer() | binary()</pre></li><li><pre>Count = integer() | binary()</pre></li><li><pre>Status = integer() | binary()</pre></li><li><pre>Articles = [#article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}]</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="list_of_usr-2"></a>

###list_of_usr/2##


<pre>list_of_usr(UsrId, Count) -&gt; {ok, Articles} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | binary()</pre></li><li><pre>Count = integer() | binary()</pre></li><li><pre>Articles = [#article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}]</pre></li><li><pre>Reason = atom()</pre></li></ul>

<a name="lookup_id-1"></a>

###lookup_id/1##


<pre>lookup_id(ArticleId) -&gt; {ok, Article} | {error, not_found}</pre>
<ul class="definitions"><li><pre>ArticleId = binary() | integer()</pre></li><li><pre>Article = #article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}</pre></li></ul>

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

<a name="to_tuple-1"></a>

###to_tuple/1##


<pre>to_tuple(Article) -&gt; TupleArticle</pre>
<ul class="definitions"><li><pre>Article = #article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>TupleArticle = tuple()</pre></li></ul>

parse usr for json object.<a name="update-1"></a>

###update/1##


<pre>update(Article) -&gt; {ok, UpdatedArticle} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Article = #article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>UpdatedArticle = #article{id = undefined | binary(), usr_id = undefined | integer(), team_id = undefined | integer(), title = undefined | binary(), text = undefined | binary(), type = undefined | integer(), created_at = undefined | non_neg_integer(), status = undefined | integer(), progress = undefined | integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

