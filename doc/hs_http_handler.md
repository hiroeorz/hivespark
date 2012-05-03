

#Module hs_http_handler#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Behaviours:__ [`cowboy_http_handler`](cowboy_http_handler.md).

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td>HTTP Handler
===================================================================.</td></tr><tr><td valign="top"><a href="#handle-5">handle/5</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>HTTP CallBacks
===================================================================.</td></tr><tr><td valign="top"><a href="#reply-2">reply/2</a></td><td></td></tr><tr><td valign="top"><a href="#reply-4">reply/4</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="handle-2"></a>

###handle/2##




`handle(Req, State) -> any()`



HTTP Handler
===================================================================<a name="handle-5"></a>

###handle/5##




<pre>handle(Controller, Action, ParamList, Req, State) -&gt; {ok, Req2, NewState}</pre>
<ul class="definitions"><li><pre>Controller = binary()</pre></li><li><pre>Action = binary()</pre></li><li><pre>ParamList = [tuple()]</pre></li><li><pre>Req = tuple()</pre></li><li><pre>State = tuple()</pre></li><li><pre>Req2 = tuple()</pre></li><li><pre>NewState = tuple()</pre></li></ul>

<a name="init-3"></a>

###init/3##




`init(X1, Req, Opts) -> any()`



HTTP CallBacks
===================================================================<a name="reply-2"></a>

###reply/2##




`reply(Status, Req) -> any()`

<a name="reply-4"></a>

###reply/4##




`reply(Status, Headers, Body, Req) -> any()`

<a name="terminate-2"></a>

###terminate/2##




`terminate(Req, State) -> any()`

