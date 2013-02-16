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

-module(eggs_gateway_websocket).

%% API
-export([behaviour_info/1, connect/0, disconnect/0, send_to_client/1]).

-spec behaviour_info(_) -> 'undefined' | [{'connect',0} | {'disconnect',0} | {'handle_event',2} | {'send_command',1},...].
behaviour_info(callbacks) ->
  [{connect, 0}, {disconnect, 0}, {send_command, 1}, {handle_event, 2}];

behaviour_info(_) ->
  undefined.

-spec connect() -> any().
connect() ->
  % TODO
  ok.

-spec disconnect() -> any().
disconnect() ->
  % TODO
  ok.

-spec send_to_client(any()) -> any().
send_to_client(_EventMessageCoded) ->
  % TODO
  ok.
