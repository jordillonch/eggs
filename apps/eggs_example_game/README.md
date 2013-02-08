Quick Start
===
erl -pa ebin/ deps/*/ebin/ -boot example_game

example_game_app:start_new_game().


example_game_gateway_shell:start().
Cmd> start_new_game
Starting new game...
new game ok
Game1 (not logged)> login
User: jordi
Password: pass
login ok
Game1 (jordi)> load_character
Id: 1
ok
Game1 (jordi) Iguana> move
x: 10
y: 10
moving (0,0)
moving (2,2)
moving (4,4)
...
moving (10,10)
Game1 (jordi) Iguana> exit

TEST (COPY & PASTE)
===
example_game_gateway_shell:start().
start_new_game
login


load_character

move
1
1



Create boot
===
erl -pa ebin deps/*/ebin
systools:make_script("example_game").
q().

Execute boot
===
erl -pa ebin/ deps/*/ebin/ -boot example_game
