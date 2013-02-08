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

-module(eggs_gateway_shell).

%% API
-export([start/2, behaviour_info/1, state_get_property/2, state_set_property/2, state_set_property/3]).

behaviour_info(callbacks) ->
  [{handle_cmd,2}, {init, 1}];

behaviour_info(_) ->
  undefined.

start(Prompt, Module) ->
  {ok, State} = Module:init([]),
  command_line(Module, Prompt, State).

command_line(Module, Prompt, State) ->
  case io:get_line(standard_io, Prompt) of
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      command_line(Module, Prompt, State);
    {eof} ->
      command_line(Module, Prompt, State);
    Command ->
      case Module:handle_cmd(Command, State) of
        {stop, _Reason} ->
          ok;
        {ok, NewState} ->
          command_line(Module, Prompt, NewState)
      end
  end.

state_get_property(State, Property) ->
  proplists:get_value(Property, State).

state_set_property(State, Values) when is_list(Values) ->
  State ++ Values.
state_set_property(State, Property, Value) ->
  State ++ [{Property, Value}].
