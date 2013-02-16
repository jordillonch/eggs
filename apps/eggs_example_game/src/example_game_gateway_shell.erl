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

-module(example_game_gateway_shell).
-behaviour(eggs_gateway_shell).

%% API
-export([start/0]).
-export([handle_cmd/2, init/1]).

-spec start() -> 'ok'.
start() ->
  io:format("Welcome to Example Game.~n"),
  io:format("(q + ENTER to exit)~n"),
  eggs_gateway_shell:start("Cmd> ", ?MODULE),
  io:format("Bye.~n").

-spec init([]) -> {'ok',[]}.
init([]) ->
  {ok, []}.

-spec handle_cmd(_,_) -> {'ok',_} | {'stop','normal'}.
handle_cmd("\n", State) ->
  {ok, State};
handle_cmd("q\n", _State) ->
  {stop, normal};
handle_cmd("info\n", State) ->
  io:format("~p~n", [State]),
  {ok, State};
handle_cmd("info player\n", State) ->
  cmd_info_player(State);
handle_cmd("start_new_game\n", State) ->
  cmd_start_new_game(State);
handle_cmd("game_stop\n", State) ->
  cmd_game_stop(State);
handle_cmd("login\n", State) ->
  cmd_login(State);
handle_cmd("load_character\n", State) ->
  cmd_load_character(State);
handle_cmd("move\n", State) ->
  cmd_move(State);
handle_cmd(Command, State) ->
  io:format("ERROR: Command ~p not valid.~n", [Command]),
  {ok, State}.

%% eval(Expresion, Bindings) ->
%%   {ok,Scanned,_} = erl_scan:string(Expresion),
%%   {ok,Parsed} = erl_parse:parse_exprs(Scanned),
%%   {value, Resultado, _Bindings} = erl_eval:exprs(Parsed,Bindings),
%%   {add_bindings(Bindings, _Bindings), Resultado}.
%%
%% add_bindings(Bindings, []) ->
%%   Bindings;
%% add_bindings(Bindings, [{Var, Value}| Tail]) ->
%%   add_bindings(erl_eval:add_binding(Var, Value, Bindings), Tail).

-spec cmd_start_new_game(_) -> {'ok',_}.
cmd_start_new_game(State) ->
  io:format("Starting new game...~n"),
  {ok, GameServerId, GameServerPid} = example_game_command:do(start_new_game, none),
  % TODO: check if new game was started previously
  NewState = eggs_gateway_shell:state_set_property(State, [{game_server, GameServerPid}, {game_server_id, GameServerId}]),
  {ok, NewState}.

-spec cmd_game_stop(_) -> {'ok',[]}.
cmd_game_stop(State) ->
  io:format("Stopping game...~n"),
  GameServerPid = eggs_gateway_shell:state_get_property(State, game_server),
  GameServerId = eggs_gateway_shell:state_get_property(State, game_server_id),
  ok = example_game_command:do(game_stop, {GameServerPid, GameServerId}),
  NewState = [],
  {ok, NewState}.

-spec cmd_login(_) -> {'ok',_}.
cmd_login(State) ->
  NewState = case io:get_line(standard_io, "Login: ") of
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      State;
    eof -> State;
    Login ->
      case io:get_line(standard_io, "Password: ") of
        {error, Reason} ->
          io:format("error: ~p~n", [Reason]),
          State;
        eof -> State;
        Password ->
          GameServer = eggs_gateway_shell:state_get_property(State, game_server),
          case example_game_command:do(login, {GameServer, Login, Password}) of
            {ok, Session} ->
              ModifiedState = eggs_gateway_shell:state_set_property(State, session, Session),
              ModifiedState;
            _ -> State
          end
      end
  end,
  {ok, NewState}.

-spec cmd_load_character(_) -> {'ok',_}.
cmd_load_character(State) ->
  NewState = case io:get_line(standard_io, "Id: ") of
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]);
    %{eof} -> State;
    CharacterId ->
      Session = eggs_gateway_shell:state_get_property(State, session),
      {CharacterIdConv, _} = string:to_integer(CharacterId),
      case example_game_command:do(load_character, {Session, CharacterIdConv}) of
        {ok, Player} ->
          io:format("Player loaded: ~p~n", [Player]),
          eggs_gateway_shell:state_set_property(State, player, Player);
        {error, Reason} ->
          io:format("ERROR: ~p~n", [Reason]),
          State
      end
  end,
  {ok, NewState}.

-spec cmd_move(_) -> {'ok',_}.
cmd_move(State) ->
  case io:get_line(standard_io, "x: ") of
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]);
    %{eof} -> State;
    X ->
      case io:get_line(standard_io, "y: ") of
        {error, Reason} ->
          io:format("error: ~p~n", [Reason]);
        %{eof} -> State;
        Y ->
          Player = eggs_gateway_shell:state_get_property(State, player),
          {XConv, _} = string:to_integer(X),
          {YConv, _} = string:to_integer(Y),
          example_game_command:do(move, {Player, {XConv, YConv, 0, 0}})
      end
  end,
  {ok, State}.

-spec cmd_info_player(_) -> {'ok',_}.
cmd_info_player(State) ->
  Player = eggs_gateway_shell:state_get_property(State, player),
  Info = example_game_command:do(player_info, Player),
  io:format("Player: ~p~n", [Info]),
  {ok, State}.

