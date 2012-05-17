

#Module hs_message#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.

Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_msg-1">get_msg/1</a></td><td>get messages.</td></tr><tr><td valign="top"><a href="#mget_msg-1">mget_msg/1</a></td><td>multi get messages.</td></tr><tr><td valign="top"><a href="#save-1">save/1</a></td><td>save message.</td></tr><tr><td valign="top"><a href="#to_tuple-1">to_tuple/1</a></td><td>parse message for json object.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_msg-1"></a>

###get_msg/1##


<pre>get_msg(Id) -&gt; {ok, Message} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Id = integer() | binary()</pre></li><li><pre>Message = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

get messages<a name="mget_msg-1"></a>

###mget_msg/1##


<pre>mget_msg(MsgIdList) -&gt; MessageList</pre>
<ul class="definitions"><li><pre>MsgIdList = [binary()]</pre></li><li><pre>MessageList = [#message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}] | []</pre></li></ul>

multi get messages<a name="save-1"></a>

###save/1##


<pre>save(Message) -&gt; {ok, NewMessage} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Message = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>NewMessage = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>Reason = atom()</pre></li></ul>

save message<a name="to_tuple-1"></a>

###to_tuple/1##


<pre>to_tuple(Msg) -&gt; TupleMsg</pre>
<ul class="definitions"><li><pre>Msg = #message{id = undefined | integer(), usr_id = undefined | integer(), team_id = undefined | integer(), text = undefined | binary(), created_at = undefined | non_neg_integer(), lat = undefined | string(), lng = undefined | string()}</pre></li><li><pre>TupleMsg = tuple()</pre></li></ul>

parse message for json object.