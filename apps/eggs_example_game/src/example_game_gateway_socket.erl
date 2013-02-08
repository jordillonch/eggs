%% eggs (Erlang Generic Game Server)
%%
%% Copyright (C) 2012-2013  Jordi Llonch <llonch.jordi at gmail dot com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%%% usage:
%%%  {ok, GameServerId, GameServerPid, SocketPid, Session, Player} = example_game_gateway_socket:start().
%%% with 1 bot:
%%%  example_game_test_bot:run(GameServerPid, 10, 100).
%%% with N bots:
%%%  example_game_gateway_socket:test_bots(GameServerPid, 10, 5000, 10).
-module(example_game_gateway_socket).

%% API
-export([start/0, loop/2, test_bots/4]).

% echo_server specific code
start() ->
  io:format("starting new game~n"),
  {ok, GameServerId, GameServerPid} = example_game_command:do(start_new_game, none),
  World = eggs_service:get_game_service(GameServerPid, world),
  Login = "login",
  Password = "pass",
  {ok, Session} = example_game_command:do(login, {GameServerPid, Login, Password}),
  CharacterId = 1,
  {ok, Player} = example_game_command:do(load_character, {Session, CharacterId}),
  {ok, SocketPid} = socket_server:start(?MODULE, 7000, {?MODULE, loop}, {Session, Player, World}),
  {ok, GameServerId, GameServerPid, SocketPid, Session, Player}.


loop(Socket, LoopData) ->
  {_Session, Player, World} = LoopData,
  case gen_tcp:recv(Socket, 0) of
    {ok, <<"quit\r\n">>} ->
      ok;
    {ok, Data} ->
      lager:debug("~p", [Data]),

      % TODO: replace with protocol buffers

      % get new position and move player
      Tokens = string:tokens(erlang:binary_to_list(Data), ","),
      [XData, YData] = Tokens,
      XData2 = case string:str(XData, ".") of
        0 -> XData ++ ".0";
        _ -> XData
      end,
      YData2 = case string:str(YData, ".") of
        0 -> YData ++ ".0";
        _ -> YData
      end,
      lager:debug("X,Y: ~p,~p", [XData2, YData2]),
      {XDataFloat, _} = string:to_float(XData2),
      {YDataFloat, _} = string:to_float(YData2),
      lager:debug("XFloat,YFloat: ~p,~p", [XDataFloat, YDataFloat]),
      example_game_command:do(move, {Player, {XDataFloat, YDataFloat, 0, 0}}),

      % get position of other entities to send coords
      EntitiesList = example_game_world:get_entities_list(World),
      lager:debug("EntitiesList: ~p", [EntitiesList]),
      % proplist: [{EntityId, {Entity, Coords}}, ...]
      % [{#Ref<0.0.0.1661>,{{example_game_player,#Ref<0.0.0.1661>,nonode@nohost},{10,10}}},{#Ref<0.0.0.1727>,{{example_game_player,#Ref<0.0.0.1727>,nonode@nohost},{406,209}}},{#Ref<0.0.0.1790>,{{example_game_player,#Ref<0.0.0.1790>,nonode@nohost},{767,603}}}]
      PlayerEntityId = eggs_entity:get_id(Player),
      ListToSend = [erlang:ref_to_list(EntityId) ++ "=" ++ io_lib:format("~.2f",[X]) ++ "," ++ io_lib:format("~.2f",[Y]) ++ "," ++ io_lib:format("~.2f",[VectorX]) ++ "," ++ io_lib:format("~.2f",[VectorY]) || {EntityId, {_Entity, {X, Y, VectorX, VectorY}}} <- EntitiesList, EntityId =/= PlayerEntityId],
      lager:debug("ListToSend: ~p", [ListToSend]),

      % send coords
      DataToSend = string:join(ListToSend, "|"),
      lager:debug("DataToSend: ~p", [DataToSend]),
      case DataToSend of
        [] -> gen_tcp:send(Socket, "null");
        _ -> gen_tcp:send(Socket, DataToSend)
      end,
      loop(Socket, LoopData);
    {error, closed} ->
      ok
  end.

test_bots(GameServerPid, NumPlayers, NumMoves, TimeWaitMoves) ->
  List = lists:seq(1, NumPlayers),
  lists:map(fun(_A) -> spawn(example_game_test_bot, run, [GameServerPid, NumMoves, TimeWaitMoves]) end, List).
