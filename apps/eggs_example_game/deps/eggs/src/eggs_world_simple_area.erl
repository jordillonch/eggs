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

-module(eggs_world_simple_area).
-behaviour(eggs_entity).
-behaviour(eggs_trait).
-behaviour(eggs_trait_active).

%% API
-export([start_active_trait/3, initialize/1, is_active/0, destroy/1]).
-export([get/2, set/2, set/3, frozen/2, running/2, run/1, froze/1, entity_add/3, entity_remove/2, entity_move/3, entity_add_handler/3, entity_remove_handler/3, stop/1, get_entities_list/1]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{frozen, 2}, {running, 2}, {set, 3}, {set, 2}, {get, 2}, {run, 1}, {froze, 1}, {stop, 1}];
behaviour_info(_) ->
  undefined.

start_active_trait(Module, InitialState, Entity) ->
  eggs_trait_active:start_active_trait(Module, InitialState, Entity).

%% entity
initialize({GameServerPid, WorldSupPid, ModuleWorldArea, AreaSpecs}) ->
  lager:debug("Initializing world..."),
  %WorldSupPid = eggs_service:get_game_service(GameServerPid, world_sup),
  %% entity
  {ok, AreaDb} = eggs_area:initialize(AreaSpecs),
  WorldEntityInit = eggs_entity:initialize(ModuleWorldArea, [{game_server_pid, GameServerPid},
                                                             {world_sup_pid, WorldSupPid},
                                                             {area_db, AreaDb}]),
  %% trait active
  WorldEntity = eggs_trait_active:initialize_sup_pid(?MODULE, WorldSupPid, WorldEntityInit, frozen),
  {ok, WorldEntity}.

destroy(World) ->
  eggs_trait_active:destroy(World).

is_active() -> true.

%% entity
get(World, Property) ->
  eggs_trait_active:get(World, Property).
set(World, Values) ->
  eggs_trait_active:set(World, Values).
set(World, Property, Value) ->
  eggs_trait_active:set(World, Property, Value).

%% trait active handlers
frozen({running, _Message}, World) ->
  lager:debug("World state: frozen; event: running"),
  NewSession = eggs_entity:base_set(World, auth, true),
  {reply, NewSession, running, NewSession};
frozen({Event, Message}, World) ->
  lager:debug("World state: frozen. {~p, ~p}", [Event, Message]),
  Module = eggs_entity:get_module(World),
  {next_state, NewState, NewWorld} = Module:frozen({Event, Message}, World),
  {next_state, NewState, NewWorld}.

running({frozen, _Message}, World) ->
  lager:debug("World state: running; event: frozen"),
  NewWorld = eggs_entity:base_set(World, auth, true),
  {reply, NewWorld, frozen, NewWorld};
running({entity_add, {Entity, Coords}}, World) ->
  % TODO: refactor
  lager:debug("World state: running. event: entity_add {~p, ~p}", [Entity, Coords]),
  AreaDb = eggs_entity:base_get(World, area_db),
  {ok, AreaDbModified} = eggs_area:entity_add(AreaDb, Entity, Coords),
  NewWorld = eggs_entity:base_set(World, area_db, AreaDbModified),
  {next_state, running, NewWorld};
running({entity_remove, {Entity}}, World) ->
  % TODO: refactor
  lager:debug("World state: running. event: entity_remove {~p, ~p}", [Entity]),
  AreaDb = eggs_entity:base_get(World, area_db),
  {ok, AreaDbModified} = eggs_area:entity_remove(AreaDb, Entity),
  NewWorld = eggs_entity:base_set(World, area_db, AreaDbModified),
  {next_state, running, NewWorld};
running({entity_move, {Entity, Coords}}, World) ->
  % TODO: refactor
  lager:debug("World state: running. event: entity_move {~p, ~p}", [Entity, Coords]),
  AreaDb = eggs_entity:base_get(World, area_db),
  % TODO: for multiple area world, here decide if only move in the current area or remove from current area and add to another area
  {ok, AreaDbModified} = eggs_area:entity_move(AreaDb, Entity, Coords),
  NewWorld = eggs_entity:base_set(World, area_db, AreaDbModified),
  {next_state, running, NewWorld};
running({get_entities_list, _Message}, World) ->
  % TODO: refactor
  lager:debug("World state: running. event: get_entities_list"),
  AreaDb = eggs_entity:base_get(World, area_db),
  EntitiesList = eggs_area:get_entities_list(AreaDb),
  {reply, EntitiesList, running, World};
running({Event, Message}, World) ->
  lager:debug("World state: running. {~p, ~p}", [Event, Message]),
  Module = eggs_entity:get_module(World),
  {next_state, NewState, NewWorld} = Module:running({Event, Message}, World),
  {next_state, NewState, NewWorld}.


run(World) ->
  eggs_trait_active:notify_sync_event(World, running, none).
froze(World) ->
  eggs_trait_active:notify_sync_event(World, frozen, none).

entity_add(World, Entity, Coords) ->
  % TODO: refactor
  ok = eggs_trait_active:notify_event(World, entity_add, {Entity, Coords}),
  ok.

entity_remove(World, Entity) ->
  % TODO: refactor
  ok = eggs_trait_active:notify_event(World, entity_remove, {Entity}),
  ok.

entity_move(World, Entity, Coords) ->
  % TODO: refactor
  ok = eggs_trait_active:notify_event(World, entity_move, {Entity, Coords}),
  ok.

get_entities_list(World) ->
  eggs_trait_active:notify_sync_event(World, get_entities_list, none).

entity_add_handler(_World, _Entity, _Handler) ->
  % TODO
  ok.

entity_remove_handler(_World, _Entity, _Handler) ->
  % TODO
  ok.

stop(_World) -> ok.