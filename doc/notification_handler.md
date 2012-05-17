

#Module notification_handler#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.

Copyright (c) (C) 2012, Hiroe Shin

__Behaviours:__ [`cowboy_http_websocket_handler`](cowboy_http_websocket_handler.md).

__Authors:__ Hiroe Shin ([`shin@sy11.komatsuelec.co.jp`](mailto:shin@sy11.komatsuelec.co.jp)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-3">init/3</a></td><td>
initialize functions.</td></tr><tr><td valign="top"><a href="#websocket_handle-3">websocket_handle/3</a></td><td>
called when request received from http client.</td></tr><tr><td valign="top"><a href="#websocket_info-3">websocket_info/3</a></td><td>
called when message received from other process.</td></tr><tr><td valign="top"><a href="#websocket_init-3">websocket_init/3</a></td><td></td></tr><tr><td valign="top"><a href="#websocket_terminate-3">websocket_terminate/3</a></td><td>
called when client closed connection.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="init-3"></a>

###init/3##


`init(X1, Req, Opts) -> any()`


initialize functions<a name="websocket_handle-3"></a>

###websocket_handle/3##


`websocket_handle(Data, Req, State) -> any()`


called when request received from http client.<a name="websocket_info-3"></a>

###websocket_info/3##


`websocket_info(Info, Req, State) -> any()`


called when message received from other process.<a name="websocket_init-3"></a>

###websocket_init/3##


`websocket_init(TransportName, Req, Opts) -> any()`

<a name="websocket_terminate-3"></a>

###websocket_terminate/3##


`websocket_terminate(Reason, Req, State) -> any()`


called when client closed connection.