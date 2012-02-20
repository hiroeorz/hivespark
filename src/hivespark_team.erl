%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hivespark_team).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/3, delete/1, lookup_id/1, lookup_name/1]).
-export([start_child/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {id :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create new user.
%% @end
%%--------------------------------------------------------------------
-spec create(Name, IconUrl, Description) -> {ok, Team} | {error, Reason} when
      Name :: binary(),
      IconUrl :: binary(),
      Description :: binary(),
      Team :: #team{},
      Reason :: atom().
create(Name, IconUrl, Description) ->
    case hivespark_team_db:lookup_name(Name) of
        {ok, _Team} ->
            {error, already_exist};
        {error, not_found} ->
            hivespark_team_db:insert(Name, IconUrl, Description)
    end.

%%--------------------------------------------------------------------
%% @doc delete user.
%% @end
%%--------------------------------------------------------------------
-spec delete(TeamId) -> {ok, deleted} | {error, not_found} when
      TeamId :: integer() | string() | binary().
delete(TeamId) ->
    case hivespark_team_db:lookup_id(TeamId) of
        {error, not_found} ->
            {error, not_found};
        {ok, _Team} ->
            hivespark_team_cache:delete(TeamId),
            {ok, deleted} = hivespark_team_db:delete(TeamId),
            {ok, deleted}
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(TeamId) -> {ok, Team} | {error, not_found} when
      TeamId :: integer() | list() | binary(),
      Team :: #team{}.
lookup_id(TeamId) ->
    case hivespark_team_cache:lookup_id(TeamId) of
        {ok, Team} -> {ok, Team};
        {error, not_found} ->
            case hivespark_team_db:lookup_id(TeamId) of
                {error, not_found} -> {error, not_found};
                {ok, Team} -> 
                    hivespark_team_cache:store(Team),
                    {ok, Team}
            end
    end.

%%--------------------------------------------------------------------
%% @doc lookup user by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Team} | {error, not_found} when
      Name :: string(),
      Team :: #team{}.
lookup_name(Name) ->
    case hivespark_team_cache:lookup_name(Name) of
        {ok, Team} -> {ok, Team};
        {error, not_found} ->
            case hivespark_team_db:lookup_name(Name) of
                {error, not_found} -> {error, not_found};
                {ok, Team} -> 
                    hivespark_team_cache:store(Team),
                    {ok, Team}
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server under the supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_child(TeamId) -> {ok, Pid} | {error, Reason} when
      TeamId :: integer(),
      Pid :: pid(),
      Reason :: atom().
start_child(TeamId) ->
    case lookup_id(TeamId) of
        {error, Reason} -> {error, Reason};
        {ok, _Team} -> 
            supervisor:start_child(hivespark_team_sup,
                                   {hivespark_team_sup, 
                                    {hivespark_team, start_link, [TeamId]},
                                    permanent, 5000, worker, 
                                    [hivespark_team_sup, hivespark_team]})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(TeamId) -> {ok, Pid} | ignore | {error, Error} when
      TeamId :: integer(),
      Pid :: pid(),
      Error :: atom().
start_link(TeamId) ->
    gen_server:start_link(?MODULE, [TeamId], []).

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
init([TeamId]) ->
    {ok, #state{id = TeamId}}.

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
