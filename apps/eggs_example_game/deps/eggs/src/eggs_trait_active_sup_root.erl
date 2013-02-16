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

-module(eggs_trait_active_sup_root).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, start_active_entity_sup/2]).

%% Starts an active entity root supervisor
%% We will start a supervisor for every type of active entities, supervised by this one
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
  lager:debug("Starting EGGS active entity root supervisor..."),
  supervisor:start_link(?MODULE, []).

-spec init([]) -> {'ok',{{'one_for_one',0,1},[]}}.
init([]) ->
  StartSpecs = {{one_for_one, 0, 1},[]},
  {ok, StartSpecs}.

%% Starts a supervisor for an active entity of a specific type
-spec start_active_entity_sup(atom() | pid() | {atom(),atom()},_) -> {'ok','undefined' | pid()}.
start_active_entity_sup(TraitActiveSupRootPid, EntityName) ->
  %% Starts a supervisor for this type of entity if not exists
  TraitActiveSupSpec = {EntityName, {eggs_trait_active_sup, start_link, [EntityName]}, permanent, 2000, supervisor, []},
  {ok, SupPid} = supervisor:start_child(TraitActiveSupRootPid, TraitActiveSupSpec),
  {ok, SupPid}.

