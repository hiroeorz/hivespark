%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Feb 2012 by Hiroe Shin <shin@sy11.komatsuelec.co.jp>
%%%-------------------------------------------------------------------
-module(contents_server).

-behaviour(gen_server).

%% Include
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).
-export([shared_dir/0, get_file_path/2, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 公開静的ファイルを置くディレクトリへのパスを返す。
-spec shared_dir() -> Path when 
      Path :: string().
shared_dir() ->
    {ok, Dir} = priv_dir_path(),
    Dir ++ "/priv/www".

%% @doc ベースパスとパスのリストから連結した絶対パスを返す。
-spec get_file_path(Path, PathList) -> TotalPath when
      Path :: string(),
      PathList :: list(),
      TotalPath :: string().
get_file_path(Path, []) ->
    Path;

get_file_path(Path, [Filename | Tail]) ->
   get_file_path(Path ++ "/" ++ Filename, Tail).

reload() ->
    gen_server:call(?SERVER, {reload}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ok = load_shared_files(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({reload}, _From, State) ->
    ets:delete(share),
    Reply = load_shared_files(),
    {reply, Reply, State};    

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc 起動時に呼ばれ、静的ファイルをetsに読み込む。
-spec load_shared_files() -> ok.
load_shared_files() ->
    SharedDir = shared_dir(),
    {ok, Filenames} = file:list_dir(SharedDir),
    ets:new(shared, [set, protected, named_table]),
    load_files_in_dir(Filenames, SharedDir),
    ok.

%% @doc 指定ディレクトリ内のファイルを再帰的にetsに読み込む。
-spec load_files_in_dir(Filenames, Dir) -> ok when
      Filenames :: list(),
      Dir :: string().
load_files_in_dir([], _Dir) -> 
    ok;

load_files_in_dir([Filename | Tail], Dir) ->
    Path = Dir ++ "/" ++ Filename,
    {ok, FileInfo} = file:read_file_info(Path),

    case FileInfo#file_info.type of
        directory ->
            {ok, Filenames} = file:list_dir(Path),
            load_files_in_dir(Filenames, Path),
            load_files_in_dir(Tail, Dir);
        regular ->
            ok = load_file(Path),
            load_files_in_dir(Tail, Dir);
        _Else ->
            load_files_in_dir(Tail, Dir)
    end.

-spec load_file(Path) -> ok | {error, enoent} when 
      Path :: string().
load_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> 
            true = ets:insert(shared, {Path, Bin}),
            ok;
        {error, enoent} ->
            {error, enoent}
    end.

-spec priv_dir_path() -> string().
priv_dir_path() ->
    case code:priv_dir(my_cowboy) of
        {error, bad_name} -> file:get_cwd();
        Dir -> Dir
    end.
