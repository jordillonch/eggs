-module(socket_server).
-author('Jesse E.I. Farmer <jesse@20bits.com>').
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/4]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
  port,
  loop,
  loop_data,
  ip=any,
  lsocket=null}).

-spec start(atom(),_,_,_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start(Name, Port, Loop, LoopData) ->
  State = #server_state{port = Port, loop = Loop, loop_data = LoopData},
  gen_server:start_link({local, Name}, ?MODULE, State, []).

-spec init(#server_state{port::char()}) -> {'ok',#server_state{}} | {'stop',atom()}.
init(State = #server_state{port=Port}) ->
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSocket} ->
      NewState = State#server_state{lsocket = LSocket},
      {ok, accept(NewState)};
    {error, Reason} ->
      {stop, Reason}
  end.

-spec handle_cast({'accepted',_},#server_state{}) -> {'noreply',#server_state{}}.
handle_cast({accepted, _Pid}, State=#server_state{}) ->
  {noreply, accept(State)}.

-spec accept_loop({atom() | pid() | {atom(),_} | {'via',_,_},port(),{atom() | tuple(),atom()},_}) -> any().
accept_loop({Server, LSocket, {M, F}, LoopData}) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  % Let the server spawn a new process and replace this loop
  % with the echo loop, to avoid blocking
  gen_server:cast(Server, {accepted, self()}),
  M:F(Socket, LoopData).

% To be more robust we should be using spawn_link and trapping exits
-spec accept(#server_state{}) -> #server_state{}.
accept(State = #server_state{lsocket=LSocket, loop = Loop, loop_data = LoopData}) ->
  proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop, LoopData}]),
  State.

% These are just here to suppress warnings.
-spec handle_call(_,_,_) -> {'noreply',_}.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_Msg, Library) -> {noreply, Library}.
-spec terminate(_,_) -> 'ok'.
terminate(_Reason, _Library) -> ok.
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.