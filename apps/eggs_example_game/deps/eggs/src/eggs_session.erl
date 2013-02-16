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

-module(eggs_session).
-behaviour(eggs_entity).
-behaviour(eggs_trait).
-behaviour(eggs_trait_active).

%% API
-export([start_active_trait/3, initialize/1, is_active/0, get_game_server_pid/1, destroy/1]).
-export([get/2, set/2, set/3, not_auth/2, auth/2, notify_auth_ok/1, notify_auth_ko/1, stop/1]).
-export([behaviour_info/1]).

-spec behaviour_info(_) -> 'undefined' | [{'auth',2} | {'get',2} | {'get_game_server_pid',1} | {'not_auth',2} | {'set',2 | 3},...].
behaviour_info(callbacks) ->
  [{not_auth, 2}, {auth, 2}, {set, 3}, {set, 2}, {get, 2}, {get_game_server_pid, 1}];
behaviour_info(_) ->
  undefined.

-spec start_active_trait(_,_,_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_active_trait(Module, InitialState, Entity) ->
  eggs_trait_active:start_active_trait(Module, InitialState, Entity).

%% entity
-spec initialize({pid(),_}) -> {'ok',{atom(),_,_}}.
initialize({GameServerPid, Module}) ->
  lager:debug("Initializing session..."),
  SessionSupPid = eggs_service:get_game_service(GameServerPid, session_sup),
  %% entity
  SessionEntityInit = eggs_entity:initialize(Module, [{game_server_pid, GameServerPid},
                                                      {session_sup_pid, SessionSupPid},
                                                      {auth, false}]),
  %% trait active
  SessionEntity = eggs_trait_active:initialize_sup_pid(?MODULE, SessionSupPid, SessionEntityInit, not_auth),
  {ok, SessionEntity}.

-spec destroy({atom(),_,_}) -> 'ok'.
destroy(Session) ->
  eggs_trait_active:destroy(Session).

-spec is_active() -> 'true'.
is_active() -> true.

-spec get_game_server_pid({atom(),_,_}) -> any().
get_game_server_pid(Session) ->
  get(Session, game_server_pid).

%% entity
-spec get({atom(),_,_},_) -> any().
get(Session, Property) ->
  eggs_trait_active:get(Session, Property).
-spec set(_,[{_,_}]) -> any().
set(Session, Values) ->
  eggs_trait_active:set(Session, Values).
-spec set({atom(),_,_},_,_) -> {atom(),_,_}.
set(Session, Property, Value) ->
  eggs_trait_active:set(Session, Property, Value).

%% trait active handlers
-spec not_auth({_,_},{_,_,_}) -> {'next_state','auth' | 'not_auth',_} | {'reply',{atom(),_,_},'auth',{atom(),_,_}}.
not_auth({auth_ok, _Message}, Session) ->
  lager:debug("Session state: not auth; event: auth_ok"),
  NewSession = eggs_entity:base_set(Session, auth, true),
  {reply, NewSession, auth, NewSession};
not_auth({Event, Message}, Session) ->
  lager:debug("Session state: not auth. {~p, ~p}", [Event, Message]),
  Module = eggs_entity:get_module(Session),
  case Module:not_auth({Event, Message}, Session) of
    {next_state, not_auth, NewSession} -> {next_state, not_auth, NewSession};
    {next_state, auth, NewSession} -> {next_state, auth, NewSession}
  end.

-spec auth({_,_},{_,_,_}) -> {'next_state','auth' | 'not_auth',_} | {'reply',{atom(),_,_},'not_auth',{atom(),_,_}}.
auth({auth_ko, _Message}, Session) ->
  lager:debug("Session state: auth; event: auth_ko"),
  NewSession = eggs_entity:base_set(Session, auth, false),
  {reply, NewSession, not_auth, NewSession};
auth({Event, Message}, Session) ->
  lager:debug("Session state: auth. {~p, ~p}", [Event, Message]),
  Module = eggs_entity:get_module(Session),
  case Module:auth({Event, Message}, Session) of
    {next_state, not_auth, NewSession} -> {next_state, not_auth, NewSession};
    {next_state, auth, NewSession} -> {next_state, auth, NewSession}
  end.


-spec notify_auth_ok({atom(),_,_}) -> any().
notify_auth_ok(Session) ->
  eggs_trait_active:notify_sync_event(Session, auth_ok, none).
-spec notify_auth_ko({atom(),_,_}) -> any().
notify_auth_ko(Session) ->
  eggs_trait_active:notify_sync_event(Session, auth_ko, none).

-spec stop(_) -> 'ok'.
stop(_Session) -> ok.

% TODO: heartbeat

