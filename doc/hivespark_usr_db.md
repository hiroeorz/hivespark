

#Module hivespark_usr_db#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net`](mailto:shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>delete usr from database.</td></tr><tr><td valign="top"><a href="#insert-6">insert/6</a></td><td>insert new usr to database.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>get usr list from user id list.</td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup user by id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup user by name.</td></tr><tr><td valign="top"><a href="#update-7">update/7</a></td><td>update user parameter.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="delete-1"></a>

###delete/1##




<pre>delete(UsrId) -&gt; {ok, deleted} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | string() | binary()</pre></li></ul>



delete usr from database.<a name="insert-6"></a>

###insert/6##




<pre>insert(Name, LongName, Mail, Password, IconUrl, Description) -&gt; {ok, Usr} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = binary()</pre></li><li><pre>LongName = binary()</pre></li><li><pre>Mail = binary()</pre></li><li><pre>Password = binary()</pre></li><li><pre>IconUrl = binary()</pre></li><li><pre>Description = binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | tuple()}</pre></li><li><pre>Reason = atom()</pre></li></ul>



insert new usr to database.<a name="list-1"></a>

###list/1##




<pre>list(UsrIdList) -&gt; UsrList</pre>
<ul class="definitions"><li><pre>UsrIdList = [integer() | string() | binary()]</pre></li><li><pre>UsrList = [#usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | tuple()} | undefined]</pre></li></ul>



get usr list from user id list.<a name="lookup_id-1"></a>

###lookup_id/1##




<pre>lookup_id(UsrId) -&gt; {ok, Usr} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UsrId = integer() | list() | binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | tuple()}</pre></li></ul>



lookup user by id.<a name="lookup_name-1"></a>

###lookup_name/1##




<pre>lookup_name(Name) -&gt; {ok, Usr} | {error, not_found}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | tuple()}</pre></li></ul>



lookup user by name.<a name="update-7"></a>

###update/7##




<pre>update(UsrId, Name, LongName, Mail, Password, IconUrl, Description) -&gt; {ok, Usr} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UsrId = integer()</pre></li><li><pre>Name = binary()</pre></li><li><pre>LongName = binary()</pre></li><li><pre>Mail = binary()</pre></li><li><pre>Password = binary()</pre></li><li><pre>IconUrl = binary()</pre></li><li><pre>Description = binary()</pre></li><li><pre>Usr = #usr{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), description = string(), created_at = undefined | tuple()}</pre></li><li><pre>Reason = atom()</pre></li></ul>



update user parameter.