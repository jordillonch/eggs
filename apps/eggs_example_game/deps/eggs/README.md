Create boot
===
erl -pa ebin deps/*/ebin
systools:make_script("eggs").
q().

Execute boot
===
erl -pa ebin/ deps/*/ebin/ -boot eggs

Game shell
===
example_game_shell:start().

TODO
===
eggs_trait_active use messages for accessors (too many messages between processes)
