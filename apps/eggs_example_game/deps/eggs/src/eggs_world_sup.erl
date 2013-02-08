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

-module(eggs_world_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% supervisor
-export([init/1, start_world/1]).

%% API
start_link(GameServerPid, World) ->
  lager:debug("Starting world supervisor..."),
  {ok, WorldSupPid} = supervisor:start_link(?MODULE, []),
  {ok, WorldInit} = World:initialize({GameServerPid, WorldSupPid}),
  {ok, WorldSupPid, WorldInit}.


%% supervisor callbacks
init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.

start_world(WorldSupPid) ->
  lager:debug("Starting new world..."),
  {ok, WorldPid} = supervisor:start_child(WorldSupPid, []),
  {ok, WorldPid}.

