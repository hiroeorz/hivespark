

#Module hivespark_websocket_handler#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Behaviours:__ [`cowboy_http_websocket_handler`](cowboy_http_websocket_handler.md).

__Authors:__ Hiroe Shin ([`shin@sy11.komatsuelec.co.jp`](mailto:shin@sy11.komatsuelec.co.jp)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-3">init/3</a></td><td>HTTP CallBacks
===================================================================.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#websocket_handle-3">websocket_handle/3</a></td><td>WebSocket Handler
===================================================================.</td></tr><tr><td valign="top"><a href="#websocket_info-3">websocket_info/3</a></td><td></td></tr><tr><td valign="top"><a href="#websocket_init-3">websocket_init/3</a></td><td>WebSocket CallBacks
===================================================================.</td></tr><tr><td valign="top"><a href="#websocket_terminate-3">websocket_terminate/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="init-3"></a>

###init/3##




`init(X1, Req, Opts) -> any()`



HTTP CallBacks
===================================================================<a name="terminate-2"></a>

###terminate/2##




`terminate(Req, State) -> any()`

<a name="websocket_handle-3"></a>

###websocket_handle/3##




`websocket_handle(Any, Req, State) -> any()`



WebSocket Handler
===================================================================<a name="websocket_info-3"></a>

###websocket_info/3##




`websocket_info(Info, Req, State) -> any()`

<a name="websocket_init-3"></a>

###websocket_init/3##




`websocket_init(Any, Req, X3) -> any()`



WebSocket CallBacks
===================================================================<a name="websocket_terminate-3"></a>

###websocket_terminate/3##




`websocket_terminate(Reason, Req, State) -> any()`

