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

%%% TODO: refactor using quadtrees and ETS
-module(eggs_area).

%% API
-export([initialize/1, entity_add/3, entity_remove/2, entity_move/3, get_entities_list/1]).

initialize(AreaSpecs) ->
  % init event manager
  {ok, EventManagerPid} = gen_event:start_link(),
%%   {ok, EventManagerPid} = gen_event:start(),
  % AreaSpecs = [{x, 0}, {y, 0}, {width, 1000}, {height, 1000}]
  % AreaData = {AreaSpecs, EntityList}
  AreaDb = {EventManagerPid, AreaSpecs, []},
  {ok, AreaDb}.


entity_add(AreaDb, Entity, Coords) ->
  {EventManagerPid, AreaSpecs, EntityList} = AreaDb,
  EntityId = eggs_entity:get_id(Entity),
  NewEntityList = EntityList ++ [{EntityId, {Entity, Coords}}],
  NewAreaDb = {EventManagerPid, AreaSpecs, NewEntityList},
  % add handler to entity
  HandlerId = {eggs_area_handler, eggs_entity:get_id(Entity)},
  gen_event:add_sup_handler(EventManagerPid, HandlerId, Entity),
  % notify to this area that a new entity has been added
  gen_event:notify(EventManagerPid, {entity_added, Entity, Coords}),
  {ok, NewAreaDb}.

entity_remove(AreaDb, Entity) ->
  {EventManagerPid, AreaSpecs, EntityList} = AreaDb,
  EntityId = eggs_entity:get_id(Entity),
  NewEntityList = proplists:delete(EntityId, EntityList),
  NewAreaDb = {EventManagerPid, AreaSpecs, NewEntityList},
  % remove handler
  HandlerId = {eggs_area_handler, eggs_entity:get_id(Entity)},
  gen_event:delete_handler(EventManagerPid, HandlerId, []),
  % notify to this area that this entity has been removed
  gen_event:notify(EventManagerPid, {entity_removed, Entity}),
  {ok, NewAreaDb}.

entity_move(AreaDb, Entity, Coords) ->
  {EventManagerPid, AreaSpecs, EntityList} = AreaDb,
  EntityId = eggs_entity:get_id(Entity),
  NewEntityList = proplists:delete(EntityId, EntityList),
  NewEntityList2 = NewEntityList ++ [{EntityId, {Entity, Coords}}],
  NewAreaDb = {EventManagerPid, AreaSpecs, NewEntityList2},
  % notify to this area this that this entity has been moved
  gen_event:notify(EventManagerPid, {entity_moved, Entity, Coords}),
  {ok, NewAreaDb}.

get_entities_list(AreaDb) ->
  {_EventManagerPid, _AreaSpecs, EntityList} = AreaDb,
  EntityList. % proplist
