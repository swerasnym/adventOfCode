-module(aoc2015_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day3_ex.txt", star1, 4},
        {"examples/2015/day3_ex.txt", star2, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Directions) ->
    Deliveries = deliver(Directions, {0, 0}, []),
    maps:size(tools:count(Deliveries)).

star2(Directions) ->
    EnumeratedDirs = lists:enumerate(Directions),
    Santa = [D || {N, D} <- EnumeratedDirs, N rem 2 == 0],
    Robot = [D || {N, D} <- EnumeratedDirs, N rem 2 == 1],

    Deliveries = deliver(Santa, {0, 0}, []) ++ deliver(Robot, {0, 0}, []),
    maps:size(tools:count(Deliveries)).

read(File) ->
    tools:read_string(File).

deliver([], Pos, Delivered) ->
    [Pos | Delivered];
deliver([Dir | Rest], Pos, Delivered) ->
    deliver(Rest, move(Dir, Pos), [Pos | Delivered]).

move($^, {X, Y}) -> {X, Y - 1};
move($v, {X, Y}) -> {X, Y + 1};
move($>, {X, Y}) -> {X + 1, Y};
move($<, {X, Y}) -> {X - 1, Y}.
