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

-module(example_game_gateway_websocket).
-behaviour(eggs_gateway_websocket).

%% API
-export([connect/0, disconnect/0, send_command/1, handle_event/2]).

connect() ->
  eggs_lobby_websocket:connect().

disconnect() ->
  eggs_lobby_websocket:disconnect().

send_command(CommandData) ->
  {Command, Data} = example_game_json:decode(CommandData),
  example_game_command:do(Command, Data).

handle_event(_Event, EventMessage) ->
  EventMessageCoded = example_game_json:encode(EventMessage),
  eggs_lobby_websocket:send_to_client(EventMessageCoded).

