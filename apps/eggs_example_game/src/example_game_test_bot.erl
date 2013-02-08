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

-module(example_game_test_bot).

%% API
-export([run/3]).

%% TimeWaitMoves = milliseconds
run(GameServer, NumMoves, TimeWaitMoves) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),

  Login = "login",
  Password = "pass",
  {ok, Session} = example_game_command:do(login, {GameServer, Login, Password}),
  CharacterId = 1,
  {ok, Player} = example_game_command:do(load_character, {Session, CharacterId}),

  List = lists:seq(1, NumMoves),
  %lists:map(fun(_A) -> random_moves(Player, TimeWaitMoves, 500, 500) end, List),
%%   NewXDelta = random:uniform(10)/100,
%%   NewYDelta = random:uniform(10)/100,
%%   NewXDelta2 = case random:uniform(2) of
%%     1 -> NewXDelta;
%%     2 -> -1 * NewXDelta
%%   end,
%%   NewYDelta2 = case random:uniform(2) of
%%     1 -> NewYDelta;
%%     2 -> -1 * NewYDelta
%%   end,
  NewXDelta2 = 0.1,
  NewYDelta2 = 0.1,
  AngleDelta = (2 * math:pi() / 360) * random:uniform(360),
  lists:foldl(fun(_A, {ok, {X, Y, XDelta, YDelta, Angle, Iteration}}) -> random_moves(Player, TimeWaitMoves, X, Y, XDelta, YDelta, Angle, Iteration) end, {ok, {500, 500, NewXDelta2, NewYDelta2, AngleDelta, 0}}, List),
%%   lists:foldl(fun(_A, {ok, {X, Y, XDelta, YDelta, Iteration}}) -> random_moves(Player, TimeWaitMoves, X, Y, XDelta, YDelta, 0, Iteration) end, {ok, {500, 500, NewXDelta2, NewYDelta2, 51}}, List),

% unload player
  ok = example_game_command:do(player_unload, Player),

  % destroy session
  ok = example_game_command:do(session_destroy, Session),

  ok.

random_moves(Player, TimeWaitMoves, XInit, YInit, _XDelta, _YDelta, Angle, Iteration) when Iteration > 5 ->
  NewAngleDelta = (2 * math:pi() / 3600) * random:uniform(20),
  NewAngle = Angle + NewAngleDelta,
  NewXDelta = erlang:round(math:cos(NewAngle) * 10) / 100,
  NewYDelta = erlang:round(math:sin(NewAngle) * 10) / 100,
  random_moves(Player, TimeWaitMoves, XInit, YInit, NewXDelta, NewYDelta, NewAngle, 0);
%% %%   NewXDelta = XDelta + random:uniform(10)/100,
%% %%   NewYDelta = YDelta + random:uniform(10)/100,
%%   NewXDelta = random:uniform(10)/1000,
%%   NewYDelta = random:uniform(10)/1000,
%%   NewXDelta2 = case random:uniform(2) of
%%     1 -> XDelta + NewXDelta;
%%     2 -> XDelta - NewXDelta
%%   end,
%%   NewYDelta2 = case random:uniform(2) of
%%     1 -> YDelta + NewYDelta;
%%     2 -> YDelta - NewYDelta
%%   end,
%%   random_moves(Player, TimeWaitMoves, XInit, YInit, NewXDelta2, NewYDelta2, Angle, 0);
random_moves(Player, TimeWaitMoves, XInit, YInit, XDelta, YDelta, Angle, Iteration) ->
  X = XInit + XDelta,
  Y = YInit + YDelta,
  timer:sleep(TimeWaitMoves),
  example_game_command:do(move, {Player, {X, Y, XDelta, YDelta}}),
%%   {ok, {X, Y, XDelta, YDelta, Iteration+1}}.
  {ok, {X, Y, XDelta, YDelta, Angle, Iteration+1}}.
