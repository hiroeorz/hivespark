%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 21 Feb 2012 by Hiroe Shin <shin@u657207.xgsfmg28.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(hs_session).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([create/1, get_value/2, set_value/3, del_value/2, get_usr/1, 
         check_loggedin/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(SESSION_KEY_HEADER, "_hs_session_").

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

%%--------------------------------------------------------------------
%% @doc get session value from key.
%% @end
%%--------------------------------------------------------------------
-spec get_value(SessionKey, Key) -> {ok, Val} | {error, not_found} when
      SessionKey :: binary(),
      Key :: string() | binary() | integer(),
      Val :: binary().
get_value(SessionKey, Key) ->
    case eredis_pool:q(?DB_SRV, ["HGET", SessionKey, Key]) of
        {ok, undefined} -> {error, not_found};
        {ok, Val} -> {ok, Val}
    end.

%%--------------------------------------------------------------------
%% @doc set session value with key.
%% @end
%%--------------------------------------------------------------------
-spec set_value(SessionKey, Key, Val) -> ok when
      SessionKey :: binary(),
      Key :: string() | binary() | integer(),
      Val :: binary() | string() | integer().
set_value(SessionKey, Key, Val) ->
    {ok, _} = eredis_pool:q(?DB_SRV, ["HSET", SessionKey, Key, Val]),
    ok.

%%--------------------------------------------------------------------
%% @doc delete session value with key.
%% @end
%%--------------------------------------------------------------------
-spec del_value(SessionKey, Key) -> ok when
      SessionKey :: binary(),
      Key :: string() | binary() | integer().
del_value(SessionKey, Key) ->
    {ok, _} = eredis_pool:q(?DB_SRV, ["HDEL", SessionKey, Key]),
    ok.

%%--------------------------------------------------------------------
%% @doc get usr record.
%% @end
%%--------------------------------------------------------------------
-spec get_usr(SessionKey) -> Usr | undefined when
      SessionKey :: binary(),
      Usr :: #usr{}.
get_usr(SessionKey) ->
    case get_value(SessionKey, <<"usr_id">>) of
        {error, _} -> undefined;
        {ok, UsrId} -> 
            case hs_usr:lookup_id(UsrId) of
                {error, not_found} -> undefined;
                {ok, Usr} -> Usr
            end
    end.

%%--------------------------------------------------------------------
%% @doc check already loggedin user?
%% @end
%%--------------------------------------------------------------------
-spec check_loggedin(UsrId, SessionKey) -> true | false when
      UsrId :: binary() | integer(),
      SessionKey :: binary().
check_loggedin(UsrId, SessionKey) when is_integer(UsrId) ->
    check_loggedin(list_to_binary(integer_to_list(UsrId)), SessionKey);

check_loggedin(UsrId, SessionKey) when is_binary(UsrId) ->
    case get_value(SessionKey, <<"usr_id">>) of
        undefined -> false;
        {ok, UsrIdFromSession} ->
            case UsrIdFromSession of
                UsrId -> true;
                _ -> false
            end
    end.

%%--------------------------------------------------------------------
%% @doc ceate new session, and return session key.
%% @end
%%--------------------------------------------------------------------
-spec create(Usr) -> {ok, SessionKey} |  {error, already_exist} | {error, Else} when
      Usr :: #usr{},
      SessionKey :: binary(),
      Else :: tuple().
create(Usr) ->
    gen_server:call(?SERVER, {create, Usr}).

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
handle_call({create, Usr}, _From, State) ->
    SessionKey = create_session_key(Usr),

    Reply =  case get_value(SessionKey, <<"usr_id">>) of
                 {ok, _} -> {error, already_exist};
                 {error, not_found} ->
                     Result = eredis_pool:q(?DB_SRV, 
                                            ["HSET", SessionKey, <<"usr_id">>, Usr#usr.id]), 
                     case Result of
                         {ok, _} -> {ok, SessionKey};
                         Else -> {error, Else}
                     end
             end,

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

-spec create_session_key(Usr) -> SessionKey when
      Usr :: #usr{},
      SessionKey :: binary().
create_session_key(Usr) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = Usr#usr.created_at,
    {{YearNow, MonthNow, DayNow}, {HourNow, MinNow, SecNow}} = {date(), time()},

    TimeStr = integer_to_list(Year) ++ integer_to_list(Month) ++
        integer_to_list(Day) ++ integer_to_list(Hour) ++
        integer_to_list(Min) ++ float_to_list(Sec),
    
    TimeNowStr = integer_to_list(YearNow) ++ integer_to_list(MonthNow) ++
        integer_to_list(DayNow) ++ integer_to_list(HourNow) ++
        integer_to_list(MinNow) ++ integer_to_list(SecNow),
    
    SessionKeyBin = crypto:sha([TimeStr, TimeNowStr,
                                binary_to_list(Usr#usr.name),
                                binary_to_list(Usr#usr.email)]),
    
    S = lists:flatten(lists:map(fun(X) -> io_lib:format("~.16X", [X, ""]) end, 
                                binary_to_list(SessionKeyBin))),
    list_to_binary(?SESSION_KEY_HEADER ++ string:substr(S, 1, 32)).
