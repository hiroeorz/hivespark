%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_team).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([create/4, update/1, delete/1, lookup_id/1, lookup_name/1, 
         checkin_members/1, checkin/3, checkout/3, to_tuple/1, 
         is_member/2, is_owner/2,
         add_message/1, get_messages/3, get_members/1, get_articles/4,
         add_article/1, statuses_list/2]).

-export([start_child/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(CHECKIN_TEAM_ID, "checkin_team_id").
-define(TEAM_TIMELINE, <<"_htt_">>).

-record(state, {id :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create new team.
%% @end
%%--------------------------------------------------------------------
-spec create(Name, OwnerId, IconUrl, Description) -> 
                    {ok, Team} | {error, Reason} when
      Name :: binary(),
      OwnerId :: integer() | binary(),
      IconUrl :: binary(),
      Description :: binary(),
      Team :: #team{},
      Reason :: atom().
create(Name, OwnerId, IconUrl, Description) when is_binary(OwnerId) ->
    create(Name, list_to_integer(binary_to_list(OwnerId)), 
           IconUrl, Description);

create(Name, OwnerId, IconUrl, Description) when is_integer(OwnerId) ->
    case hs_team_db:lookup_name(Name) of
        {ok, _Team} -> {error, already_exist};
        {error, not_found} -> 
            {ok, Team} = hs_team_db:insert(Name, OwnerId, IconUrl, Description),
            hs_usr:add_team(OwnerId, Team#team.id),
            {ok, Team}
    end.

%%--------------------------------------------------------------------
%% @doc update team.
%% @end
%%--------------------------------------------------------------------
-spec update(Team) -> {ok, Team} | {error, Reason} when
      Team :: #team{},
      Reason :: atom().
update(Team) ->
    hs_team_db:update(Team).

%%--------------------------------------------------------------------
%% @doc delete team.
%% @end
%%--------------------------------------------------------------------
-spec delete(TeamId) -> {ok, deleted} | {error, not_found} when
      TeamId :: integer() | string() | binary().
delete(TeamId) ->
    case hs_team_db:lookup_id(TeamId) of
        {error, not_found} ->
            {error, not_found};
        {ok, _Team} ->
            {ok, deleted} = hs_team_db:delete(TeamId),
            {ok, deleted}
    end.

%%--------------------------------------------------------------------
%% @doc lookup team by id.
%% @end
%%--------------------------------------------------------------------
-spec lookup_id(TeamId) -> {ok, Team} | {error, not_found} when
      TeamId :: integer() | list() | binary(),
      Team :: #team{}.
lookup_id(TeamId) ->
    hs_team_db:lookup_id(TeamId).

%%--------------------------------------------------------------------
%% @doc lookup team by name.
%% @end
%%--------------------------------------------------------------------
-spec lookup_name(Name) -> {ok, Team} | {error, not_found} when
      Name :: string(),
      Team :: #team{}.
lookup_name(Name) ->
    hs_team_db:lookup_name(Name).

%%--------------------------------------------------------------------
%% @doc checkin usr in team room.
%% @end
%%--------------------------------------------------------------------
-spec checkin(SessionKey, TeamId, UsrId) -> {ok, Members} | {error, Reason} when
      SessionKey :: binary(),
      TeamId :: integer() | binary(),
      UsrId :: integer() | binary(),
      Members :: [#usr{}],
      Reason :: atom().
checkin(SessionKey, TeamId, UsrId) ->
    case hs_team:lookup_id(TeamId) of
        {error, not_found} -> {error, team_not_found};
        {ok, _} ->
            case hs_usr_db:lookup_id(UsrId) of
                {error, not_found} -> {error, usr_not_found};
                {ok, _} ->
                    TeamRoomKey = team_room_key(TeamId), 
                    {ok, _} = eredis_pool:q(?DB_SRV, ["LREM", TeamRoomKey, 0, UsrId]),
                    {ok, _} = eredis_pool:q(?DB_SRV, ["RPUSH", TeamRoomKey, UsrId]),

                    case hs_session:get_value(SessionKey, ?CHECKIN_TEAM_ID) of
                        {error, not_found} -> ok;
                        {ok, TId} -> checkout(SessionKey, TId, UsrId)
                    end,

                    ok = hs_session:set_value(SessionKey, ?CHECKIN_TEAM_ID, TeamId),
                    {ok, checkin_members(TeamId)}
            end
    end.

%%--------------------------------------------------------------------
%% @doc checkout usr from team room.
%% @end
%%--------------------------------------------------------------------
-spec checkout(SessionKey, TeamId, UsrId) -> {ok, Members} | {error, Reason} when
      SessionKey :: binary(),
      TeamId :: integer() | binary(),
      UsrId :: integer() | binary(),
      Members :: [#usr{}],
      Reason :: atom().
checkout(SessionKey, TeamId, UsrId) ->
    TeamRoomKey = team_room_key(TeamId), 
    {ok, _} = eredis_pool:q(?DB_SRV, ["LREM", TeamRoomKey, 0, UsrId]),
    ok = hs_session:del_value(SessionKey, ?CHECKIN_TEAM_ID),
    {ok, checkin_members(TeamId)}.

%%--------------------------------------------------------------------
%% @doc all checkin members in team room.
%% @end
%%--------------------------------------------------------------------
-spec checkin_members(TeamId) -> Members when
      TeamId :: integer() | binary(),
      Members :: [#usr{}].
checkin_members(TeamId) ->
    TeamRoomKey = team_room_key(TeamId),
    case eredis_pool:q(?DB_SRV, ["LRANGE", TeamRoomKey, 0, -1]) of
        {ok, []} -> [];
        {ok, UsrIds} ->
            Members = hs_usr_cache:list(UsrIds),
            lists:filter(fun(M) -> M =/= undefined end, Members)
    end.

%%--------------------------------------------------------------------
%% @doc check added Usr is member of Team.
%% @end
%%--------------------------------------------------------------------
-spec is_member(TeamId, UsrId) -> true | false when
      TeamId :: integer(),
      UsrId :: integer().
is_member(TeamId, UsrId) ->
    {ok, Members} = get_members(TeamId),
    lists:any(fun(M) -> M#usr.id == UsrId end, Members).

%%--------------------------------------------------------------------
%% @doc check added Usr is owner of Team.
%% @end
%%--------------------------------------------------------------------
-spec is_owner(TeamId, UsrId) -> true | false when
      TeamId :: integer(),
      UsrId :: integer().
is_owner(TeamId, UsrId) ->
    case lookup_id(TeamId) of
        {error, not_found} -> false;
        {ok, Team} ->
            case Team#team.owner_id of
                UsrId -> true;
                _ -> false
            end
    end.

%%--------------------------------------------------------------------
%% @doc parse team for json object.
%% @end
%%--------------------------------------------------------------------
-spec to_tuple(Team) -> TupledTeam when
      Team :: #team{} | integer(),
      TupledTeam :: [tuple()].
to_tuple(TeamId) when is_integer(TeamId) ->
    case lookup_id(TeamId) of
        {error, Reason} -> {error, Reason};
        {ok, Team} -> to_tuple(Team)
    end;

to_tuple(Team) ->
    {ok, Owner} = hs_usr:lookup_id(Team#team.owner_id),
    {ok, Members} = get_members(Team#team.id),

    {[{id, Team#team.id}, {owner, hs_usr:to_tuple(Owner)},
      {members, lists:map(fun(U) -> hs_usr:to_tuple(U) end, Members)},
      {name, Team#team.name}, {icon_url, Team#team.icon_url},
      {status, Team#team.status},
      {status_description, Team#team.status_description},
      {description, Team#team.description}]}.

%%--------------------------------------------------------------------
%% @doc add message to users timeline.
%% @end
%%--------------------------------------------------------------------
-spec add_message(Msg) -> {ok, Msg2} | {error, Reason} when
      Msg :: #message{},
      Msg2 :: #message{},
      Reason :: atom().
add_message(Msg=#message{usr_id=UsrId, text=Text, team_id=TeamId}) when 
      is_binary(TeamId),
      is_integer(UsrId),
      is_binary(Text) ->
    TeamIdInt = list_to_integer(binary_to_list(TeamId)),
    add_message(Msg#message{team_id = TeamIdInt});

add_message(Msg=#message{usr_id=UsrId, text=Text, team_id=TeamId}) when 
      is_integer(TeamId),
      is_integer(UsrId),
      is_binary(Text) ->
    case hs_message:save(Msg) of
        {error, Reason} -> {error, Reason};
        {ok, NewMsg} -> 
            ok = add_message_id(TeamId, NewMsg#message.id),
            {ok, NewMsg}
    end.

-spec add_article(Art) -> {ok, Art2} | {error, Reason} when
      Art :: #article{},
      Art2 :: #article{}, 
      Reason :: atom().
add_article(Art=#article{usr_id=UsrId, text=Text, team_id=TeamId}) when 
      is_binary(TeamId),
      is_integer(UsrId),
      is_binary(Text) ->
    TeamIdInt = list_to_integer(binary_to_list(TeamId)),
    add_message(Art#article{team_id = TeamIdInt});

add_article(Art=#article{usr_id=UsrId, text=Text, team_id=TeamId}) when 
      is_integer(TeamId),
      is_integer(UsrId),
      is_binary(Text) ->
    hs_article_db:insert(Art).

%%--------------------------------------------------------------------
%% @doc add message to users timeline.
%% @end
%%--------------------------------------------------------------------
-spec get_messages(TeamId, Offset, Count) -> 
                          {ok, MessageList} | {error, not_found}  when
      TeamId :: integer() | binary(),
      Offset :: integer(),
      Count :: integer(),
      MessageList :: [#message{}] | [].
get_messages(TeamId, Offset, Count) when is_binary(TeamId) ->
    get_messages(list_to_integer(binary_to_list(TeamId)), Offset, Count);

get_messages(TeamId, Offset, Count) when is_integer(TeamId) ->
    case get_message_ids(TeamId, Offset, Count) of
        {ok, []} ->
            spawn(fun() -> load_messages_to_cache(TeamId) end),
            hs_message_db:list_of_team(TeamId, Offset, Count);
        {ok, Ids} -> {ok, hs_message:mget_msg(Ids)}
    end.        

%%--------------------------------------------------------------------
%% @doc get team list of given status.
%% @end
%%--------------------------------------------------------------------
-spec statuses_list(Lebel, Count) -> [TeamList] when
      Lebel :: integer(),
      Count :: integer(),
      TeamList :: [] | [#team{}].
statuses_list(Level, Count) ->
    hs_team_db:statuses_list(Level, Count).

%%--------------------------------------------------------------------
%% @doc add message to users timeline.
%% @end
%%--------------------------------------------------------------------
-spec get_articles(TeamId, Offset, Count, Status) -> 
                          {ok, ArticleList} | {error, not_found}  when
      TeamId :: integer() | binary(),
      Offset :: integer(),
      Count :: integer(),
      Status :: integer(),
      ArticleList :: [#article{}] | [].
get_articles(TeamId, Offset, Count, Status) when is_binary(TeamId) ->
    get_articles(list_to_integer(binary_to_list(TeamId)), Offset, 
                 Count, Status);

get_articles(TeamId, Offset, Count, Status) when is_integer(TeamId) ->
    hs_article_db:list_of_team(TeamId, Offset, Count, Status).

%%--------------------------------------------------------------------
%% @doc add message to users timeline.
%% @end
%%--------------------------------------------------------------------
-spec get_members(TeamId) -> {ok, UsrList} | {error, Reason} when
      TeamId :: integer() | binary(),
      UsrList :: [#usr{}],
      Reason :: atom().
get_members(TeamId) when is_binary(TeamId) ->
    get_members(list_to_integer(binary_to_list(TeamId)));

get_members(TeamId) when is_integer(TeamId) ->
    hs_usr_team_db:get_teams_usrs(TeamId).

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
            {ok, _Pid} = supervisor:start_child(hs_team_sup,
                                               {hs_team_sup, 
                                                {hs_team, start_link, [TeamId]},
                                                permanent, 5000, worker, 
                                                [hs_team_sup, hs_team]})
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
-spec team_room_key(TeamId) -> Key when
      TeamId :: integer() | string() | binary(),
      Key :: binary().
team_room_key(TeamId) when is_binary(TeamId) -> 
    team_room_key(binary_to_list(TeamId));

team_room_key(TeamId) when is_integer(TeamId) -> 
    team_room_key(integer_to_list(TeamId));

team_room_key(TeamId) when is_list(TeamId) ->
    list_to_binary(lists:flatten(["team_checkin_members_", TeamId])).

-spec get_key_of_timeline(TeamId) -> TimelineKey when
      TeamId :: integer(),
      TimelineKey :: binary().
get_key_of_timeline(TeamId) ->
    list_to_binary(lists:flatten([?TEAM_TIMELINE, integer_to_list(TeamId)])).

-spec add_message_id(TeamId, MsgId) -> ok when
      TeamId :: integer(),
      MsgId :: binary().
add_message_id(TeamId, MsgId) when is_integer(TeamId), is_binary(MsgId) ->
    Key = get_key_of_timeline(TeamId),
    {ok, _} = eredis_pool:q(?DB_SRV, ["LPUSH", Key, MsgId]),
    ok.

-spec get_message_ids(TeamId, Offset, Count) -> 
                          {ok, MsgIdList} | {error, not_found} when
      TeamId :: integer(),
      Offset :: integer(),
      Count :: integer(),
      MsgIdList :: [binary()].
get_message_ids(TeamId, Offset, Count) ->
    StartPos = 0 - Offset - 1,
    EndPos = 0 - Count,
    Key = get_key_of_timeline(TeamId),

    case eredis_pool:q(?DB_SRV, ["LRANGE", Key, EndPos, StartPos]) of
        {ok, undefined} -> {error, not_found};
        {ok, MsgIdList} -> {ok, MsgIdList}
    end.

-spec load_messages_to_cache(TeamId) -> ok when
      TeamId :: integer() | binary().
load_messages_to_cache(TeamId) ->
    io:format("loading team's messages (team_id=~p)~n", [TeamId]),
    {ok, Messages} = hs_message_db:list_of_team(TeamId, 0, 3000),
    lists:map(fun(M) ->
                      hs_message_cache:save(M),
                      add_message_id(TeamId, M#message.id)
              end, Messages),
    io:format("team's messages loaded (team_id=~p).~n", [TeamId]),
    ok.
