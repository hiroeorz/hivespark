%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_file).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([start_link/0, stop/0]).
-export([save/2, read/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {s3 :: tuple(), bucket :: string()}).

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
    case load_amazon_s3_keys() of
        {error, Reason} -> {error, Reason};
        {ok, [Keys]} ->
            AccessKey = proplists:get_value(access_key, Keys),
            AccessSecretKey = proplists:get_value(access_secret_key, Keys),
            BucketName = proplists:get_value(bucket, Keys),
            gen_server:start_link({local, ?SERVER}, ?MODULE, 
                                  [AccessKey, AccessSecretKey, BucketName], [])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Amazon S3へデータを保存します
%% @end
%%--------------------------------------------------------------------
-spec save(Key, Val) -> ok | {error, Reason} when
      Key :: string(),
      Val :: binary(),
      Reason :: atom().
save(Key, Val) when is_list(Key) and is_binary(Val) ->
    gen_server:call(?SERVER, {save, Key, Val}).

%%--------------------------------------------------------------------
%% @doc
%% Amazon S3からデータを読み出します
%% @end
%%--------------------------------------------------------------------
-spec read(Key) -> {ok, Content, Res} | {error, Reason} when
      Key :: string(),
      Content :: binary(),
      Res :: [tuple()],
      Reason :: atom().
read(Key) when is_list(Key) ->
    gen_server:call(?SERVER, {read, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Amazon S3からデータを削除します
%% @end
%%--------------------------------------------------------------------
-spec delete(Key) -> ok | {error, Reason} when
      Key :: string(),
      Reason :: atom().
delete(Key) when is_list(Key) ->
    gen_server:call(?SERVER, {delete, Key}).

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
init([AccessKey, AccessSecretKey, Bucket]) ->
    S3 = erlcloud_s3:new(AccessKey, AccessSecretKey),
    ok = erlcloud_s3:create_bucket(Bucket, S3),
    {ok, #state{s3 = S3, bucket = Bucket}}.

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
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({save, Key, Val}, From, State) ->
    spawn(fun() ->
                  Bucket = State#state.bucket,
                  S3 = State#state.s3,

                  Reply = 
                      try
                          S3Result = erlcloud_s3:put_object(Bucket, Key, Val, 
                                                            S3),
                          VersionId = proplists:get_value(version_id, S3Result),
                          {ok, VersionId}
                          catch
                              error:{s3_error, ErrCode, ErrMsg} ->
                                  {s3_error, ErrCode, ErrMsg}
                          end,
                      
                  gen_server:reply(From, Reply)
          end),

    {noreply, State};

handle_call({read, Key}, From, State) ->
    spawn(fun() ->
                  Bucket = State#state.bucket,
                  S3 = State#state.s3,
                  
                  Reply = 
                      try
                          S3Result = erlcloud_s3:get_object(Bucket, Key, S3),
                          Content = proplists:get_value(content, S3Result),
                          {ok, Content, S3Result}
                      catch
                          error:{aws_error, ErrMsg} -> {aws_error, ErrMsg}
                      end,
                  
                  gen_server:reply(From, Reply)
          end),

    {noreply, State};

handle_call({delete, Key}, From, State) ->
    spawn(fun() ->
                  Bucket = State#state.bucket,
                  S3 = State#state.s3,
                  
                  Reply = 
                      try
                          S3Result = erlcloud_s3:delete_object(Bucket, Key, S3),
                          VersionId = proplists:get_value(version_id, S3Result),
                          {ok, VersionId}
                      catch
                          error:{aws_error, ErrMsg} -> {aws_error, ErrMsg}
                      end,

                  gen_server:reply(From, Reply)
          end),

    {noreply, State}.

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
-spec load_amazon_s3_keys() -> {ok, Keys} | {error, Reason} when
      Keys :: [tuple()],
      Reason :: atom().
load_amazon_s3_keys() ->
    Path = hs_util:priv_dir() ++ "amazon/s3.key",
    file:consult(Path).
