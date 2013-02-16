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

-spec behaviour_info(_) -> 'undefined' | [{'froze' | 'frozen' | 'get' | 'run' | 'running' | 'set' | 'stop',1 | 2 | 3},...].
behaviour_info(callbacks) ->
  [{frozen, 2}, {running, 2}, {set, 3}, {set, 2}, {get, 2}, {run, 1}, {froze, 1}, {stop, 1}];
behaviour_info(_) ->
  undefined.

-spec start_active_trait(_,_,_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_active_trait(Module, InitialState, Entity) ->
  eggs_trait_active:start_active_trait(Module, InitialState, Entity).

%% entity
-spec initialize({_,atom() | pid() | {atom(),atom()},_,_}) -> {'ok',{atom(),_,_}}.
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

-spec destroy({atom(),_,_}) -> 'ok'.
destroy(World) ->
  eggs_trait_active:destroy(World).

-spec is_active() -> 'true'.
is_active() -> true.

%% entity
-spec get({atom(),_,_},_) -> any().
get(World, Property) ->
  eggs_trait_active:get(World, Property).
-spec set(_,[{_,_}]) -> any().
set(World, Values) ->
  eggs_trait_active:set(World, Values).
-spec set({atom(),_,_},_,_) -> {atom(),_,_}.
set(World, Property, Value) ->
  eggs_trait_active:set(World, Property, Value).

%% trait active handlers
-spec frozen({_,_},{_,_,_}) -> {'next_state',_,_} | {'reply',{atom(),_,_},'running',{atom(),_,_}}.
frozen({running, _Message}, World) ->
  lager:debug("World state: frozen; event: running"),
  NewSession = eggs_entity:base_set(World, auth, true),
  {reply, NewSession, running, NewSession};
frozen({Event, Message}, World) ->
  lager:debug("World state: frozen. {~p, ~p}", [Event, Message]),
  Module = eggs_entity:get_module(World),
  {next_state, NewState, NewWorld} = Module:frozen({Event, Message}, World),
  {next_state, NewState, NewWorld}.

-spec running({_,_},{_,_,_}) -> {'next_state',_,_} | {'reply',_,'frozen' | 'running',{atom(),_,_}}.
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


-spec run({atom(),_,_}) -> any().
run(World) ->
  eggs_trait_active:notify_sync_event(World, running, none).
-spec froze({atom(),_,_}) -> any().
froze(World) ->
  eggs_trait_active:notify_sync_event(World, frozen, none).

-spec entity_add({atom(),_,_},_,_) -> 'ok'.
entity_add(World, Entity, Coords) ->
  % TODO: refactor
  ok = eggs_trait_active:notify_event(World, entity_add, {Entity, Coords}),
  ok.

-spec entity_remove({atom(),_,_},_) -> 'ok'.
entity_remove(World, Entity) ->
  % TODO: refactor
  ok = eggs_trait_active:notify_event(World, entity_remove, {Entity}),
  ok.

-spec entity_move({atom(),_,_},_,_) -> 'ok'.
entity_move(World, Entity, Coords) ->
  % TODO: refactor
  ok = eggs_trait_active:notify_event(World, entity_move, {Entity, Coords}),
  ok.

-spec get_entities_list({atom(),_,_}) -> any().
get_entities_list(World) ->
  eggs_trait_active:notify_sync_event(World, get_entities_list, none).

-spec entity_add_handler(_,_,_) -> 'ok'.
entity_add_handler(_World, _Entity, _Handler) ->
  % TODO
  ok.

-spec entity_remove_handler(_,_,_) -> 'ok'.
entity_remove_handler(_World, _Entity, _Handler) ->
  % TODO
  ok.

-spec stop(_) -> 'ok'.
stop(_World) -> ok.