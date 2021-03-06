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
-export([save/1, get_msg/1, mget_msg/1, to_tuple/1]).

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
save(Message=#message{usr_id=_UsrId, team_id=_TeamId, text=Text}) when 
      is_binary(Text) ->
    
    NewMessage = Message#message{created_at={date(), time()}},

    case hs_message_db:insert(NewMessage) of
        {error, Reason} -> {error, Reason};
        {ok, NewMessage2} -> hs_message_cache:save(NewMessage2)
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
            NewList = lists:delete(undefined, List),
            ?debugVal(KeyList),
            ?debugVal(NewList),
            repaire_msg_list_from_db(MsgIdList, NewList)
    end.

%%--------------------------------------------------------------------
%% @doc Redis上のキャッシュに抜けがあった場合にDBから取得した値を新たにキャッシュする。
%% @end
%%--------------------------------------------------------------------
-spec repaire_msg_list_from_db(MsgIdList, MsgList) -> NewMsgList when
      MsgIdList :: [binary()],
      MsgList :: [#message{} | undefined],
      NewMsgList :: [#message{}].
repaire_msg_list_from_db(MsgIdList, MsgList) ->
    repaire_msg_list_from_db(MsgIdList, MsgList, []).

repaire_msg_list_from_db([], _, Results) ->
    lists:reverse(Results);

repaire_msg_list_from_db([MsgId | IdTail], [], Results) ->
    io:format("repareing cache message from db (2.~n"),

    case hs_message_db:get_msg(MsgId) of
                {ok, NewMsg} ->
                    spawn(fun() -> hs_message_cache:save(NewMsg) end),
                    repaire_msg_list_from_db(IdTail, [], [NewMsg | Results]);
                {error, _} ->
                    repaire_msg_list_from_db(IdTail, [], Results)
    end;

repaire_msg_list_from_db([MsgId | IdTail], [Msg | MsgTail], Results) ->
    case Msg of
        undefined ->
            io:format("repareing cache message from db (1.~n"),
            case hs_message_db:get_msg(MsgId) of
                {ok, NewMsg} ->
                    spawn(fun() -> hs_message_cache:save(NewMsg) end),
                    repaire_msg_list_from_db(IdTail, MsgTail, [NewMsg | Results]);
                {error, _} ->
                    repaire_msg_list_from_db(IdTail, MsgTail, Results)
            end;
        OkMsg ->
            repaire_msg_list_from_db(IdTail, MsgTail, [OkMsg | Results])
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

