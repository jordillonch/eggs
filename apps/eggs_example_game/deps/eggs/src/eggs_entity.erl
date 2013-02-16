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

-module(eggs_entity).

%% API
-export([behaviour_info/1]).
-export([base_get/2, base_set/2, base_set/3]).
-export([preinitialize/1, initialize/2, get/2, set/2, set/3, get_module/1, get_id/1]).

-spec behaviour_info(_) -> 'undefined' | [{'get',2} | {'initialize',1} | {'set',2 | 3},...].
behaviour_info(callbacks) ->
    [{initialize, 1}, {set, 3}, {set, 2}, {get, 2}];

behaviour_info(_) ->
    undefined.

-spec initialize(_,[{_,_}]) -> {_,reference(),atom()}.
initialize(Module, Data) ->
  lager:debug("Initializing entity ~p", [Module]),
  % entity is only a reference that allow query ets table
  % node() is used to ensure that entity is only used in the same node. If you need to use entity between nodes you must use eggs_trait_active
  InternalId = make_ref(),
  Node = erlang:node(),
  Entity = {Module, InternalId, Node},
  % write init data to ets
  base_set(Entity, Data),
  % return reference
  Entity.

-spec preinitialize(atom()) -> 'ok'.
preinitialize(Module) ->
  Entity = {Module, none, none},
  TableName = ets_table_name(Entity),
  ets:new(TableName, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]),
  ok.

-spec ets_table_name({atom(),_,_}) -> atom().
ets_table_name(Entity) ->
  {Module, _InternalId, _Node} = Entity,
  list_to_atom("eggs_entity_" ++ atom_to_list(Module)).

-spec ets_property_name({atom(),_,_},_) -> any().
ets_property_name(Entity, Property) ->
  {_Module, InternalId, _Node} = Entity,
  binary_to_list(term_to_binary(InternalId)) ++ Property.

%% api used by callback module
-spec base_get({atom(),_,_},_) -> any().
base_get(Entity, Property) ->
  % todo: check node
  TableName = ets_table_name(Entity),
  PropertyName = ets_property_name(Entity, Property),
  case ets:lookup(TableName, PropertyName) of
    [] -> not_found;
    [{_, Value}] -> Value
  end.

-spec base_set(_,[{_,_}]) -> any().
base_set(Entity, []) -> Entity;
base_set(Entity, [{Property, Value} | T]) ->
  base_set(Entity, Property, Value),
  base_set(Entity, T).

-spec base_set({atom(),_,_},_,_) -> {atom(),_,_}.
base_set(Entity, Property, Value) ->
  % todo: check node
  TableName = ets_table_name(Entity),
  PropertyName = ets_property_name(Entity, Property),
  ets:insert(TableName, {PropertyName, Value}),
  Entity.

%% public api
-spec get_id({_,_,_}) -> any().
get_id(Entity) ->
  {_Module, InternalId, _Node} = Entity,
  InternalId.

-spec get({atom() | tuple(),_,_},_) -> any().
get(Entity, Property) ->
  {Module, _InternalId, _Node} = Entity,
  Module:get(Entity, Property).

-spec set({atom() | tuple(),_,_},_) -> any().
set(Entity, Values) ->
  {Module, _InternalId, _Node} = Entity,
  Module:set(Entity, Values).
-spec set({atom() | tuple(),_,_},_,_) -> any().
set(Entity, Property, Value) ->
  {Module, _InternalId, _Node} = Entity,
  Module:set(Entity, Property, Value).

-spec get_module({_,_,_}) -> any().
get_module(Entity) ->
  {Module, _InternalId, _Node} = Entity,
  Module.
