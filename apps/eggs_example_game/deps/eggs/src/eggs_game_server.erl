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

-module(eggs_game_server).
-behaviour(supervisor).

%% API
-export([start_link/1, game_data_lookup/2, game_data_delete_table/1]).

%% supervisor
-export([init/1]).

%% API
start_link(GameConf) ->
  lager:debug("Starting EGGS game server..."),
  {ok, GameServerPid} = supervisor:start_link(?MODULE, GameConf),
  ok = start_game(GameServerPid, GameConf),
  {ok, GameServerPid}.


%% supervisor callbacks
init(GameConf) ->
  % preinitialize entities
  % if they are initialized here, process owner is the game server supervisor so if we stop game server, ets tables are deleted
  Entities = proplists:get_value(eggs_arg_entities, GameConf),
  preinitialize_entities(Entities),
  eggs_entity:preinitialize(proplists:get_value(eggs_arg_session, GameConf)),
  eggs_entity:preinitialize(proplists:get_value(eggs_arg_world, GameConf)),

  {ok, {{one_for_one, 5, 10}, []}}.

start_game(GameServerPid, GameConf) ->
  % start session supervisor
  SessionSup = {eggs_session_sup, {eggs_session_sup, start_link, []}, permanent, 2000, supervisor, dynamic},
  {ok, SessionSupPid} = supervisor:start_child(GameServerPid, SessionSup),

  % start active entities supervisors root and subsupervisors for every type of active entity
  {ok, TraitActiveSupRootPid} = start_trait_active_sup_root(GameServerPid, GameConf),
  ActiveEntitiesSupList = start_active_entities(TraitActiveSupRootPid, proplists:get_value(eggs_arg_entities, GameConf)),


  % start gateways

  % start world supervisor
  World = proplists:get_value(eggs_arg_world, GameConf),
  WorldSup = {eggs_world_sup, {eggs_world_sup, start_link, [GameServerPid, World]}, permanent, 2000, supervisor, dynamic},
  {ok, WorldSupPid, WorldInit} = supervisor:start_child(GameServerPid, WorldSup),

  % save game data
  GameData = [
%%     {system, System},
%%     {system_init_result, SystemInitResult},
%%     {world, World},
%%     {world_init_result, WorldInitResult},
    {session_sup, SessionSupPid},
    {world_sup, WorldSupPid},
    {world, WorldInit},
    {world_module, World},
    {active_entities_root_sup, TraitActiveSupRootPid},
    {active_entities_sups, ActiveEntitiesSupList}
  ],
  game_data_save(GameServerPid, GameData),

  % start system
  System = proplists:get_value(eggs_arg_system, GameConf),
  _SystemInitResult = System:initialize(GameServerPid), % TODO

  % response
  ok.

start_trait_active_sup_root(GameServerPid, _GameConf) ->
%% start TraitActiveSupRoot supervisor
%%   GamesServiceSup = {'eggs_games_service', {eggs_games_service, start_link, []},
%%     permanent, 2000, worker, dynamic},
%%
%%   SystemServiceSup = {'eggs_systems_service', {eggs_systems_service, start_link, []},
%%     permanent, 2000, worker, dynamic},
  TraitActiveSupRoot = {eggs_trait_active_sup_root, {eggs_trait_active_sup_root, start_link, []},
    permanent, 2000, supervisor, dynamic},
  {ok, TraitActiveSupRootPid} = supervisor:start_child(GameServerPid, TraitActiveSupRoot),
  {ok, TraitActiveSupRootPid}.

start_active_entities(_TraitActiveSupRootPid, []) -> [];
start_active_entities(TraitActiveSupRootPid, [EntityName | EntitiesNames]) ->
  IsActiveEntity = eggs_trait_active:is_active_entity(EntityName),
  {ok, Pid} = start_active_entities(TraitActiveSupRootPid, EntityName, IsActiveEntity),
  [{EntityName, Pid} | start_active_entities(TraitActiveSupRootPid, EntitiesNames)].
start_active_entities(TraitActiveSupRootPid, EntityName, true) ->
  {ok, Pid} = eggs_trait_active_sup_root:start_active_entity_sup(TraitActiveSupRootPid, EntityName),
  {ok, Pid};
start_active_entities(_TraitActiveSupRootPid, _EntityName, _) ->
  {ok, []}.

preinitialize_entities([]) -> ok;
preinitialize_entities([Entity | Entities]) ->
  eggs_entity:preinitialize(Entity),
  preinitialize_entities(Entities).

%% TODO: game data based on entity
game_data_table_name(GameServerPid) ->
  list_to_atom("eggs_game_data_" ++ pid_to_list(GameServerPid)).

game_data_delete_table(GameServerPid) ->
  TableName = game_data_table_name(GameServerPid),
  ets:delete(TableName).

game_data_save(GameServerPid, GameData) ->
  TableName = game_data_table_name(GameServerPid),
  ets:new(TableName, [set, named_table, public]),
  ets:insert(TableName, GameData).

game_data_lookup(GameServerPid, Key) ->
  TableName = game_data_table_name(GameServerPid),
  case ets:lookup(TableName, Key) of
    [] -> not_found;
    [{_, Value}] -> Value
  end.

