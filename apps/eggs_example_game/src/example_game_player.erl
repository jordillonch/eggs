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

-module(example_game_player).
-behaviour(eggs_entity).
-behaviour(eggs_trait).
-behaviour(eggs_trait_active).
-behaviour(eggs_trait_player). % TODO: use/implement this behaviour
-behaviour(eggs_trait_movable).

%% API
-export([start_active_trait/3, initialize/1, is_active/0, destroy/1]).
-export([get/2, set/2, set/3, configure_supervisor/0, frozen/2, info/1, stop/1]).
-export([move/2, handler_entity_moved/2, handler_entity_added/2, handler_entity_removed/1]).

start_active_trait(Module, InitialState, Entity) ->
  eggs_trait_active:start_active_trait(Module, InitialState, Entity).

%% entity
initialize({GameServerPid, CharacterSpecs}) ->
  % entity
  PlayerEntityInit = eggs_entity:initialize(?MODULE, [
    {game_server_pid, GameServerPid},
    {character_specs, CharacterSpecs}
  ]),
  % trait active
  PlayerEntity = eggs_trait_active:initialize(?MODULE, GameServerPid, PlayerEntityInit, frozen),
  % trait movable
  X = proplists:get_value(x, CharacterSpecs),
  Y = proplists:get_value(y, CharacterSpecs),
  ok = eggs_trait_movable:initialize(PlayerEntity, {X, Y}),

  PlayerEntity.

destroy(Player) ->
  eggs_trait_active:destroy(Player).

is_active() -> true.

get(Player, Property) ->
  eggs_trait_active:get(Player, Property).

set(Player, Values) ->
  eggs_trait_active:set(Player, Values).
set(Player, Property, Value) ->
  eggs_trait_active:set(Player, Property, Value).

%% trait active handlers
configure_supervisor() -> ok.


frozen(Message, Player) ->
  lager:debug("Player frozen: ~p", [Message]),
  {next_state, frozen, Player}.

%% trait player

%% trait movable
move(PlayerEntity, {X, Y, VectorX, VectorY}) ->
  eggs_trait_movable:move(PlayerEntity, {X, Y, VectorX, VectorY}).

handler_entity_moved(Entity, Coords) ->
  lager:debug("[example_game_player][handler_entity_moved]: (~p) ~p", [Coords, Entity]),
  % todo
  ok.

handler_entity_added(Entity, Coords) ->
  lager:debug("[example_game_player][handler_entity_added]: (~p) ~p", [Coords, Entity]),
  % todo
  ok.

handler_entity_removed(Entity) ->
  lager:debug("[example_game_player][handler_entity_removed]: ~p", [Entity]),
  % todo
  ok.

info(PlayerEntity) ->
  eggs_trait_active:get_loop_data(PlayerEntity).

stop(_Player) -> ok.