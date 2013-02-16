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

-module(eggs_service).
-behaviour(supervisor).

-export([start_link/0, init/1, start_game/1, get_game_service/2, game_stop/2]).

%% start game supervisor
%% inside a game supervisor it will be started the TraitActiveSupRoot...
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
  lager:debug("Starting EGGS service..."),
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

-spec init([]) -> {'ok',{{'one_for_one',3,30},[]}}.
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 3,
  MaxSecondsBetweenRestarts = 30,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, []}}.

-spec start_game(_) -> {'ok',reference(),'undefined' | pid()}.
start_game(GameConf) ->
  GameServerId = make_ref(),
  GameServerSpec = {GameServerId, {eggs_game_server, start_link, [GameConf]}, permanent, 2000, supervisor, []},
  {ok, GameServerPid} = supervisor:start_child({global, ?MODULE}, GameServerSpec),
  % todo: return as a GameServer = {GameServerId, GameServerPid}
  {ok, GameServerId, GameServerPid}.

-spec game_stop(pid(),_) -> 'ok'.
game_stop(GameServerPid, GameServerId) ->
  eggs_game_server:game_data_delete_table(GameServerPid),
  ok = supervisor:terminate_child({global, ?MODULE}, GameServerId),
  ok.

%% Service can be:
%%  session_sup
%%  active_entities_root_sup
%%  active_entities_sups
-spec get_game_service(pid(),_) -> any().
get_game_service(GameServerPid, Service) ->
  eggs_game_server:game_data_lookup(GameServerPid, Service).
