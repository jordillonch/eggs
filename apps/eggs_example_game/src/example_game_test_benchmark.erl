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

-module(example_game_test_benchmark).

%% API
-export([run/2, run_bot/3]).

-spec run(non_neg_integer(),_) -> 'ok'.
run(NumPlayers, NumMoves) ->
  io:format("starting new game~n"),
  {ok, GameServerId, GameServerPid} = example_game_command:do(start_new_game, none),
  List = lists:seq(1, NumPlayers),
  lists:map(fun(_A) -> spawn(?MODULE, run_bot, [self(), GameServerPid, NumMoves]) end, List),
  % wait until all spawned proces had finished
  io:format("waiting...~n"),
  loop(0, NumPlayers, none),
  % stop game server
  io:format("stopping game server~n"),
  ok = example_game_command:do(game_stop, {GameServerPid, GameServerId}),
  ok.

-spec loop(non_neg_integer(),non_neg_integer(),'none' | number()) -> 'ok'.
loop(NumPlayers, NumPlayers, AvgTime) ->
  AvgTimeMs = erlang:round(AvgTime/1000),
  io:format("Average time: ~p ms~n", [AvgTimeMs]),
  ok;
loop(Current, NumPlayers, none) ->
  receive
    {finished, Time} -> loop(Current + 1, NumPlayers, Time)
  end,
  ok;
loop(Current, NumPlayers, AvgTime) ->
  receive
    {finished, Time} -> loop(Current + 1, NumPlayers, erlang:round((AvgTime + Time) / 2))
  end,
  ok.


-spec run_bot(atom() | pid() | port() | {atom(),atom()},_,integer()) -> 'ok'.
run_bot(From, GameServerPid, NumMoves) ->
  T1 = now(),
  example_game_test_bot:run(GameServerPid, NumMoves, 0),
  T2 = now(),
  Dif = timer:now_diff(T2, T1),
  DifMs = erlang:round(Dif/1000),
  io:format("~p ms~n", [DifMs]),
  From ! {finished, Dif},
  ok.