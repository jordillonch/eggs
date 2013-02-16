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

-module(eggs_area_handler).
-behaviour(gen_event).

%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
  code_change/3]).

-spec init(_) -> {'ok',_}.
init(Entity) ->
  {ok, Entity}.

-spec handle_event(_,_) -> {'ok',_}.
handle_event({entity_moved, EntityMoved, Coords}, Entity) ->
  EntityModule = eggs_entity:get_module(Entity),
  EntityModule:handler_entity_moved(EntityMoved, Coords),
  {ok, Entity};
handle_event({entity_added, EntityAdded, Coords}, Entity) ->
  EntityModule = eggs_entity:get_module(Entity),
  EntityModule:handler_entity_added(EntityAdded, Coords),
  {ok, Entity};
handle_event({entity_removed, EntityRemoved}, Entity) ->
  EntityModule = eggs_entity:get_module(Entity),
  EntityModule:handler_entity_removed(EntityRemoved),
  {ok, Entity};
handle_event(_Event, Entity) ->
  {ok, Entity}.

-spec handle_call(_,_) -> {'ok','reply',_}.
handle_call(_Request, State) ->
  {ok, reply, State}.

-spec handle_info(_,_) -> {'ok',_}.
handle_info(_Info, State) ->
  {ok, State}.

-spec terminate(_,_) -> 'ok'.
terminate(_Arg, _State) ->
  ok.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
