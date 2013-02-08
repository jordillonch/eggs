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

-module(example_game_app).
-behaviour(application).

-export([start/2, stop/1, start_new_game/0, game_stop/2]).

%% start a game server
start(normal, []) ->
%%   lager:set_loglevel(lager_console_backend, debug),
  lager:set_loglevel(lager_console_backend, warning),
  lager:debug("Starting Example Game app..."),
  {ok, global:whereis_name(eggs_service)}.

start_new_game() ->
  System = example_game_system,
  World = example_game_world,
  Session = example_game_session,
  Gateways = [example_game_gateway_shell, example_game_gateway_websocket],
  Entities = [example_game_player],
  GameConf = [
    {eggs_arg_system, System},
    {eggs_arg_world, World},
    {eggs_arg_session, Session},
    {eggs_arg_gateways, Gateways},
    {eggs_arg_entities, Entities}
  ],
  {ok, GameServerId, GameServerPid} = eggs_service:start_game(GameConf),
  lager:debug("Example Game started..."),
  % run world
  WorldEntity = eggs_service:get_game_service(GameServerPid, world),
  example_game_world:run(WorldEntity),
  lager:debug("World running..."),

  {ok, GameServerId, GameServerPid}.

game_stop(GameServerPid, GameServerId) ->
  eggs_service:game_stop(GameServerPid, GameServerId).

stop(_State) ->
  ok.

