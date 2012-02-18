

#Module contents_server#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2012, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@sy11.komatsuelec.co.jp`](mailto:shin@sy11.komatsuelec.co.jp)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_file_path-2">get_file_path/2</a></td><td>ベースパスとパスのリストから連結した絶対パスを返す。.</td></tr><tr><td valign="top"><a href="#reload-0">reload/0</a></td><td></td></tr><tr><td valign="top"><a href="#shared_dir-0">shared_dir/0</a></td><td>公開静的ファイルを置くディレクトリへのパスを返す。.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_file_path-2"></a>

###get_file_path/2##




<pre>get_file_path(Path, PathList) -&gt; TotalPath</pre>
<ul class="definitions"><li><pre>Path = string()</pre></li><li><pre>PathList = list()</pre></li><li><pre>TotalPath = string()</pre></li></ul>



ベースパスとパスのリストから連結した絶対パスを返す。<a name="reload-0"></a>

###reload/0##




`reload() -> any()`

<a name="shared_dir-0"></a>

###shared_dir/0##




<pre>shared_dir() -&gt; Path</pre>
<ul class="definitions"><li><pre>Path = string()</pre></li></ul>



公開静的ファイルを置くディレクトリへのパスを返す。<a name="start_link-0"></a>

###start_link/0##




<pre>start_link() -&gt; {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the server
