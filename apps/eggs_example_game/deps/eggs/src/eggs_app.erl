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

-module(eggs_app).
-behaviour(application).

-export([
  start/2,
  stop/1
]).

-spec start(_,_) -> {'ok',pid()}.
start(_Type, _StartArgs) ->
  lager:set_loglevel(lager_console_backend, debug),
  lager:debug("Starting EGGS app..."),
  {ok, Pid} = eggs_service:start_link(),
  {ok, Pid}.

-spec stop(_) -> 'ok'.
stop(_State) ->
  ok.
