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

-module(eggs_trait_active).
-behaviour(gen_fsm).

-export([start_active_trait/3, initialize/4, initialize_sup_pid/4, get/2, set/2, set/3, destroy/1]).
-export([init/1, running/2, running/3, notify_event/3, notify_sync_event/3, is_active_entity/1, get_loop_data/1]).
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4, terminate/3]).
-export([behaviour_info/1]).

%% useful properties:
%%   trait_active_id

behaviour_info(callbacks) ->
  [{is_active, 0}, {start_active_trait, 3}, {stop, 1}];

behaviour_info(_) ->
  undefined.

%% api
start_active_trait(Module, InitialState, Entity) ->
  lager:debug("Starting active trait gen_fsm: ~p...", [Module]),
  gen_fsm:start_link(eggs_trait_active, {Module, InitialState, Entity}, []).

initialize(Module, GameServerPid, Entity, InitialState) ->
  ActiveEntitiesSupList = eggs_game_server:game_data_lookup(GameServerPid, active_entities_sups),
  EntityName = eggs_entity:get_module(Entity),
  SupPid = proplists:get_value(EntityName, ActiveEntitiesSupList),
  initialize_sup_pid(Module, SupPid, Entity, InitialState).

initialize_sup_pid(Module, SupPid, Entity, InitialState) ->
  lager:debug("Starting active trait ~p...", [Module]),
  lager:debug("Entity active trait: ~p", [Entity]),
  % start new gen fsm
  WorkerSpecs = {make_ref(), {Module, start_active_trait, [Module, InitialState, Entity]}, temporary, 2000, worker, [Module]},
  {ok, Pid} = supervisor:start_child(SupPid, WorkerSpecs),

  % save gen fsm pid in the entity
  ModifiedEntity = eggs_entity:base_set(Entity, trait_active_id, Pid),
  ok = save(ModifiedEntity, ModifiedEntity),
  % return new entity
  ModifiedEntity.

%% TODO: when entities were based on ETS, simplify accessors reading/writing directly to entity without use of events
save(_Entity, _NewEntity) ->
  % not necessary with ets because Entity is only a reference
  %gen_fsm:sync_send_event(getPidFromEntity(Entity), {save, NewEntity}).
  ok.
get(Entity, Property) ->
  %gen_fsm:sync_send_event(getPidFromEntity(Entity), {get, Property}).
  % todo: if same node get from entity directly, if not use sync_send_event
  Value = eggs_entity:base_get(Entity, Property),
  Value.
set(Entity, Values) ->
  %gen_fsm:sync_send_event(getPidFromEntity(Entity), {set, Values}).
  % todo: if same node get from entity directly, if not use sync_send_event
  eggs_entity:base_set(Entity, Values),
  Entity.
set(Entity, Property, Value) ->
  %gen_fsm:sync_send_event(getPidFromEntity(Entity), {set, {Property, Value}}).
  % todo: if same node get from entity directly, if not use sync_send_event
  eggs_entity:base_set(Entity, Property, Value),
  Entity.

get_loop_data(Entity) ->
  gen_fsm:sync_send_event(getPidFromEntity(Entity), get_loop_data).

notify_sync_event(Entity, Event, Message) ->
  gen_fsm:sync_send_event(getPidFromEntity(Entity), {Event, Message}).

notify_event(Entity, Event, Message) ->
  gen_fsm:send_event(getPidFromEntity(Entity), {Event, Message}).

is_active_entity(EntityName) ->
  EntityName:is_active().
%%   %% check
%%   case(eggs_entity:get(Entity, trait_active_id)) of
%%     undefined -> false;
%%     _ -> true
%%   end.

%% internal functions
getPidFromEntity(Entity) ->
  eggs_entity:base_get(Entity, trait_active_id).

destroy(Entity) ->
  gen_fsm:send_event(getPidFromEntity(Entity), stop).


%% heartbeat(Pid, LastTimeDiff) ->
%%   gen_fsm:send_event(Pid, {heartbeat, LastTimeDiff}).

init(LoopData) ->
  {ok, running, LoopData}.


%% async handles
running(stop, {Module, State, Data}) ->
  NewData = Module:stop(Data),
  {stop, normal, {Module, State, NewData}};
running({Event, Message}, {Module, State, Data}) ->
  R = Module:State({Event, Message}, Data),
  %lager:debug("[eggs_trait_active][running][async event]: ~p", [R]),
  case R of
    {next_state, NewState, NewData}              -> {next_state, running, {Module, NewState, NewData}};
    {next_state, NewState, NewData, hibernate}   -> {next_state, running, {Module, NewState, NewData}, hibernate};
    {next_state, NewState, NewData, Timeout}     -> {next_state, running, {Module, NewState, NewData}, Timeout};
    {stop, Reason, NewData}                      -> {stop, Reason, {Module, stopped, NewData}}
  end.

%% sync handles
running({save, NewData}, _From, {Module, State, _Data}) ->
  lager:debug("[~p][eggs_trait_active][running][save]: ~p", [Module, NewData]),
  {reply, ok, running, {Module, State, NewData}};
running({get, Property}, _From, {Module, State, Data}) ->
  lager:debug("[~p][eggs_trait_active][running][get]: ~p", [Module, Property]),
  Value = eggs_entity:base_get(Data, Property),
  {reply, Value, running, {Module, State, Data}};
running({set, {Property, Value}}, _From, {Module, State, Data}) ->
  lager:debug("[~p][eggs_trait_active][running][set]: ~p => ~p", [Module, Property, Value]),
  NewData = eggs_entity:base_set(Data, Property, Value),
  {reply, NewData, running, {Module, State, NewData}};
running({set, Values}, _From, {Module, State, Data}) ->
  lager:debug("[~p][eggs_trait_active][running][set]: ~p", [Module, Values]),
  NewData = eggs_entity:base_set(Data, Values),
  {reply, NewData, running, {Module, State, NewData}};
running(get_loop_data, _From, {Module, State, Data}) ->
  lager:debug("[~p][eggs_trait_active][running][get_loop_data]", [Module]),
  {reply, Data, running, {Module, State, Data}};
running({Event, Message}, _From, {Module, State, Data}) ->
  R = Module:State({Event, Message}, Data),
  lager:debug("[~p][eggs_trait_active][running][sync event]: ~p", [Module, R]),
  case R of
    {reply, Reply, NewState, NewData}            -> {reply, Reply, running, {Module, NewState, NewData}};
    {reply, Reply, NewState, NewData, hibernate} -> {reply, Reply, running, {Module, NewState, NewData}, hibernate};
    {reply, Reply, NewState, NewData, Timeout}   -> {reply, Reply, running, {Module, NewState, NewData}, Timeout};
    {next_state, NewState, NewData}              -> {next_state, running, {Module, NewState, NewData}};
    {next_state, NewState, NewData, hibernate}   -> {next_state, running, {Module, NewState, NewData}, hibernate};
    {next_state, NewState, NewData, Timeout}     -> {next_state, running, {Module, NewState, NewData}, Timeout};
    {stop, Reason, Reply, NewData}               -> {stop, Reason, Reply, {Module, stopped, NewData}};
    {stop, Reason, NewData}                      -> {stop, Reason, {Module, stopped, NewData}}
  end.

% todo: not used?
handle_event(stop, _StateName, {Module, State, Data}) ->
  NewData = Module:stop(Data),
  {stop, normal, {Module, State, NewData}}.

handle_info({'EXIT', _Pid, _Reason}, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
  lager:debug("handle_sync_event (~p, ~p, ~p)", [Event, StateName, StateData]),
  {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  ok.

