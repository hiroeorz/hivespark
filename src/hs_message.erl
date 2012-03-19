%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2012, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created :  7 Mar 2012 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(hs_message).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("hivespark.hrl").

%% API
-export([save/1, get_msg/1, get_next_id/1, mget_msg/1, to_tuple/1]).

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
save(Message=#message{usr_id=UsrId, team_id=_TeamId, text=Text}) when 
      is_binary(Text) ->
    MsgId = get_next_id(UsrId),
    NewMessage = Message#message{id=list_to_binary(MsgId), 
                                 created_at={date(), time()}},
    Key = get_key(MsgId),

    case eredis_pool:q(?DB_SRV, ["SET", Key, term_to_binary(NewMessage)]) of
        {ok, <<"OK">>} -> {ok, NewMessage};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc get messages
%% @end
%%--------------------------------------------------------------------
-spec get_msg(Id) -> {ok, Message} | {error, Reason} when
      Id :: integer(),
      Message :: #message{},
      Reason :: atom().
get_msg(MsgId) ->
    Key = get_key(MsgId),
    case eredis_pool:q(?DB_SRV, ["GET", Key]) of 
        {ok, undefined} -> {error, not_found};
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

%%--------------------------------------------------------------------
%% @doc parse message for json object.
%% @end
%%--------------------------------------------------------------------
-spec to_tuple(Msg) -> TupleMsg when
      Msg :: #message{},
      TupleMsg :: tuple().
to_tuple(Msg) ->
    {ok, Usr} = hs_usr:lookup_id(Msg#message.usr_id),
    DateTime = hs_util:create_datetime_string(Msg#message.created_at),
        
    {[{id, Msg#message.id}, {usr, hs_usr:to_tuple(Usr)},
      {text, Msg#message.text}, {created_at, DateTime},
      {lat, Msg#message.lat}, {lng, Msg#message.lng}]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_key(MsgId) when is_binary(MsgId) -> 
    get_key(binary_to_list(MsgId));
  
get_key(MsgId) ->
    list_to_binary(lists:flatten([?MSG_KEY_HEADER, MsgId])).

get_next_id(UsrId) when is_integer(UsrId)->
    get_next_id(integer_to_list(UsrId));

get_next_id(UsrId) when is_binary(UsrId)->
    get_next_id(binary_to_list(UsrId));

get_next_id(UsrId) when is_list(UsrId) ->
    {ok, BinVal} = 
        eredis_pool:q(?DB_SRV, ["HINCRBY", ?USR_MSG_ID, UsrId, 1]),

    UsrPart = string:right(UsrId, ?MSGID_USR_PART_LENGTH, $0),
    MsgPart = string:right(binary_to_list(BinVal), ?MSGID_MSG_PART_LENGTH, $0),
    lists:flatten([UsrPart, MsgPart]).

