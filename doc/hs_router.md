

#Module hs_router#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-3">handle/3</a></td><td>
#state{}のrequire_login=trueの場合は認証を行い、そうでない場合は認証せずに
アクション関数を呼び出す。.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="handle-3"></a>

###handle/3##




`handle(Module, Req, State) -> any()`




#state{}のrequire_login=trueの場合は認証を行い、そうでない場合は認証せずに
アクション関数を呼び出す。