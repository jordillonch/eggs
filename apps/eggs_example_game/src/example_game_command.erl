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

-module(example_game_command).
-behaviour(eggs_command).

%% API
-export([do/2]).

-spec do('game_stop' | 'load_character' | 'login' | 'move' | 'player_info' | 'player_unload' | 'session_destroy' | 'start_new_game',_) -> any().
do(start_new_game, _) ->
  example_game_app:start_new_game();

do(game_stop, {GameServer, GameServerId}) ->
  example_game_app:game_stop(GameServer, GameServerId);

do(login, {GameServer, Login, Password}) ->
  {ok, Session} = example_game_session:initialize(GameServer),
  example_game_session:login(Session, Login, Password);

do(load_character, {Session, CharacterId}) ->
  {ok, CharacterSpecs} = example_game_session:get_character(Session, CharacterId),
  GameServerPid = example_game_session:get_game_server_pid(Session),
  Player = example_game_player:initialize({GameServerPid, CharacterSpecs}),
  {ok, Player};

do(move, {Player, Data}) ->
  example_game_player:move(Player, Data);

do(player_info, Player) ->
  example_game_player:info(Player);

do(player_unload, Player) ->
  example_game_player:destroy(Player);

do(session_destroy, Session) ->
  example_game_session:destroy(Session).