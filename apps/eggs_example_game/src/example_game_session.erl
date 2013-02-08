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

-module(example_game_session).
-behaviour(eggs_session).

%% API
-export([initialize/1, login/3, get_character/2, get_game_server_pid/1, destroy/1]).
-export([not_auth/2, auth/2]).
-export([get/2, set/2, set/3]).

initialize(GameServerPid) ->
  eggs_session:initialize({GameServerPid, ?MODULE}).

destroy(Session) ->
  eggs_session:destroy(Session).

% TODO
login(Session, _Login, _Password) ->
  UserId = 1083,
  Session2 = eggs_session:set(Session, user_id, UserId),
  Session3 = eggs_session:notify_auth_ok(Session2),
  {ok, Session3}.

get_character(_Session, _CharacterId) -> % todo
  %% load character data from db
  CharacterSpecs = [
    {character_id, 128},
    {name, "Jordi"},
    {x, 10.0},
    {y, 10.0}
  ],
  {ok, CharacterSpecs}.

get_game_server_pid(Session) ->
  eggs_session:get_game_server_pid(Session).

not_auth({_Event, _Message}, Session) ->
  {next_state, not_auth, Session}.

auth({_Event, _Message}, Session) ->
  {next_state, auth, Session}.

%% entity
get(Session, Property) ->
  eggs_session:get(Session, Property).
set(Session, Values) ->
  eggs_session:set(Session, Values).
set(Session, Property, Value) ->
  eggs_session:set(Session, Property, Value).
