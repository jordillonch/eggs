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

-module(eggs_trait_movable).

-export([behaviour_info/1]).
-export([initialize/2, move/2]).

%% behaviour
-spec behaviour_info(_) -> 'undefined' | [{'handler_entity_added',2} | {'handler_entity_moved',2} | {'handler_entity_removed',1} | {'move',2},...].
behaviour_info(callbacks) ->
  [{move, 2}, {handler_entity_moved, 2}, {handler_entity_added, 2}, {handler_entity_removed, 1}];

behaviour_info(_) ->
  undefined.

%% api
-spec initialize({atom(),_,_},_) -> 'ok'.
initialize(SubjectEntity, NewPositionCoords) ->
  {World, WorldModule} = get_world_params(SubjectEntity),
  % add entity
  WorldModule:entity_add(World, SubjectEntity, NewPositionCoords),
  ok.

-spec move({atom(),_,_},_) -> 'ok'.
move(SubjectEntity, NewPositionCoords) ->
  {World, WorldModule} = get_world_params(SubjectEntity),
  % move
  ok = WorldModule:entity_move(World, SubjectEntity, NewPositionCoords),
  % notify new coord to entity
  eggs_entity:set(SubjectEntity, trait_movable_coord, NewPositionCoords),
  ok.

-spec get_world_params({atom(),_,_}) -> {_,_}.
get_world_params(SubjectEntity) ->
  % get world
  GameServerPid = eggs_entity:base_get(SubjectEntity, game_server_pid),
  World = eggs_service:get_game_service(GameServerPid, world),
  % get world module
  WorldModule = eggs_service:get_game_service(GameServerPid, world_module),
  {World, WorldModule}.