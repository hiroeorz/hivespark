%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created :  7 Mar 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_message_cache).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([save/1, get_msg/1, mget_msg/1]).

-define(MSG_KEY_HEADER, "_hmk_").
-define(USR_MSG_ID, <<"usr_msg_id">>).
-define(MSGID_USR_PART_LENGTH, 10).
-define(MSGID_MSG_PART_LENGTH, 10).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc save message
%% @end
%%--------------------------------------------------------------------

-spec save(Message) -> {ok, NewMessage} | {error, Reason} when
      Message :: #message{},
      NewMessage :: #message{},
      Reason :: atom().
save(Message=#message{id = MsgId, usr_id=_UsrId, team_id=_TeamId, text=Text}) when 
      is_binary(Text) ->

    Key = get_key(MsgId),

    case eredis_pool:q(?DB_SRV, ["SET", Key, term_to_binary(Message)]) of
        {ok, <<"OK">>} -> {ok, Message};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc get messages
%% @end
%%--------------------------------------------------------------------
-spec get_msg(Id) -> {ok, Message} | {error, Reason} when
      Id :: integer() | binary(),
      Message :: #message{},
      Reason :: atom().
get_msg(MsgId) ->
    Key = get_key(MsgId),
    case eredis_pool:q(?DB_SRV, ["GET", Key]) of 
        {ok, undefined} -> hs_message_db:get_msg(MsgId);
        {ok, MsgBin} -> {ok, binary_to_term(MsgBin)}
    end.

%%--------------------------------------------------------------------
%% @doc multi get messages
%% @end
%%--------------------------------------------------------------------
-spec mget_msg(MsgIdList) -> MessageList  when
      MsgIdList :: [binary()],
      MessageList :: [#message{}] | [].
mget_msg([]) -> [];
mget_msg(MsgIdList) when is_list(MsgIdList) ->
    KeyList = lists:map(fun(MsgId) -> get_key(MsgId) end, MsgIdList),
    case eredis_pool:q(?DB_SRV, ["MGET" | KeyList]) of 
        {ok, undefined} -> [];
        {ok, MsgBinList} -> 
            List = lists:map(fun(Data) ->
                                     case Data of
                                         undefined -> undefined;
                                         MsgBin -> binary_to_term(MsgBin)
                                     end 
                             end, 
                             MsgBinList),
            lists:delete(undefined, List)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_key(MsgId) when is_binary(MsgId) -> 
    get_key(binary_to_list(MsgId));
  
get_key(MsgId) ->
    list_to_binary(lists:flatten([?MSG_KEY_HEADER, MsgId])).

