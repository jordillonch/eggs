# eggs

**E**rlang **G**eneric **G**ame **S**erver

This project is a proof of concept that aims to be a generic framework for creating a MMORPG (*Massively Multiplayer Online Role-Playing Game*).

I take this project to learn more about **Erlang** language, my last passion :)

Video:

[![eggs with unity3d](https://i2.ytimg.com/vi/AOjHIsL-8ZA/mqdefault.jpg?v=51142d8c "eggs with unity3d")](http://youtu.be/AOjHIsL-8ZA)


## What can you find here?

The project is divided into:
### Eggs app
It provides the framework and infrastructure elements to build your game.

### Multiplayer example game
The goal is to be a "MMORPG", well, in the near future...

### Unity3D test client
It is a test to show how connect Unity3D to eggs and have a nice client to play and record videos to show ;) It's just for fun!

[eggs Unity3D test client source code](https://github.com/jordillonch/eggs_unity3d)


## Architecture

The framework provides the infrastructure elements to build your game.

The idea is that everything in the game is an "entity".
An entity can be passive or active. A passive entity is an element with some data. An active entity is a process that implements a FSM (Finite State Machine) and have some data.
Althought it could be subscribed to an event handler and be sensitive to events in the area.

Entities are implemented using ETS that ensures great performance.

All infrastructure are based on entities, so an area world, sessions, players... are active entities.

Here you can see a scheme of the framework elements:

![framework elements](https://raw.github.com/jordillonch/eggs/master/apps/eggs_example_game/docs/eggs_structure_eggs.png "Framework elements")

And here you can see a scheme of the example game

![example game](https://raw.github.com/jordillonch/eggs/master/apps/eggs_example_game/docs/eggs_structure_example_game.png "Example game")


## Benchmarks

Please don't take these results very seriously.

I have used a MacBook Pro 2.53 GHz Intel Core 2 Duo/4Gb 1067 MHz DDR3.

My first entity implementation was based on proplists but then I switched to ETS. The benchmarks compares both implementations creating N players that do M moves as quick as they can. All players are in the same area and all them are listening to the movements of others using event handlers.

The test only check the core implementation. Network connections are not tested.

### Results

The average move time with 10 players doing 1000 moves everyone, is 0.20 ms.

The average move time with 1000 players doing 1000 moves everyone, is 18 ms. This result with a i3 CPU it is reduced to only 8 ms.

[Download all results](https://raw.github.com/jordillonch/eggs/master/apps/eggs_example_game/docs/benchmarks.ods)


## Installation

Clone repository and then:

    cd <cloned dir>/apps/eggs_example_game
    make


## Quick start
### Simple shell gateway
    cd <cloned dir>/apps/eggs_example_game
    erl -pa ebin/ deps/*/ebin/ -boot example_game
    1> example_game_gateway_shell:start().
    Welcome to Example Game.
    (q + ENTER to exit)
    Cmd> start_new_game
    Starting new game...
    Cmd> login
    Login: jordi
    Password: pass
    Cmd> load_character
    Id: 1
    Player loaded: {example_game_player,#Ref<0.0.0.978>,nonode@nohost}
    Cmd> move
    x: 1
    y: 1
    Cmd> q
    Bye.
    ok

### eggs & Unity3d
    cd <cloned dir>/apps/eggs_example_game
    erl -pa ebin/ deps/*/ebin/ -boot example_game
    1> {ok, GameServerId, GameServerPid, SocketPid, Session, Player} = example_game_gateway_socket:start().
    starting new game
    {ok,#Ref<0.0.0.973>,<0.77.0>,<0.86.0>,
        {example_game_session,#Ref<0.0.0.983>,nonode@nohost},
        {example_game_player,#Ref<0.0.0.987>,nonode@nohost}}

    (START UNITY3D)

    2> example_game_gateway_socket:test_bots(GameServerPid, 10, 5000, 10).
    [<0.89.0>,<0.90.0>,<0.91.0>,<0.92.0>,<0.93.0>,<0.94.0>,
     <0.95.0>,<0.96.0>,<0.97.0>,<0.98.0>]



## Author

Jordi Llonch - llonch.jordi at gmail dot com


## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.
