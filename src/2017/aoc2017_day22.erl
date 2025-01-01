-module(aoc2017_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day22_ex.txt", star1, 5587},
        {"examples/2017/day22_ex.txt", star2, 2511944}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 22},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    {X, Y} = maps:get(max, Grid),
    Start = {X div 2, Y div 2},
    {_, _, _, Res} = tools:repeat(10000, fun step/1, {Start, north, Grid, 0}),
    Res.

star2(Grid) ->
    {X, Y} = maps:get(max, Grid),
    Start = {X div 2, Y div 2},
    {_, _, _, Res} = tools:repeat(10000000, fun step2/1, {Start, north, Grid, 0}),
    Res.

read(File) ->
    tools:read_grid(File, #{$# => infected, $. => clean}).

turn(north, left) -> west;
turn(north, right) -> east;
turn(west, left) -> south;
turn(west, right) -> north;
turn(south, left) -> east;
turn(south, right) -> west;
turn(east, left) -> north;
turn(east, right) -> south.

flip(north) -> south;
flip(south) -> north;
flip(east) -> west;
flip(west) -> east.

move(Pos, north) -> aoc_vector:add(Pos, {0, -1});
move(Pos, south) -> aoc_vector:add(Pos, {0, 1});
move(Pos, east) -> aoc_vector:add(Pos, {1, 0});
move(Pos, west) -> aoc_vector:add(Pos, {-1, 0}).

step({Pos, Dir, Map, Count}) ->
    case maps:get(Pos, Map, clean) of
        infected ->
            NDir = turn(Dir, right),
            NPos = move(Pos, NDir),
            {NPos, NDir, Map#{Pos => clean}, Count};
        clean ->
            NDir = turn(Dir, left),
            NPos = move(Pos, NDir),
            {NPos, NDir, Map#{Pos => infected}, Count + 1}
    end.

step2({Pos, Dir, Map, Count}) ->
    case maps:get(Pos, Map, clean) of
        clean ->
            NDir = turn(Dir, left),
            NPos = move(Pos, NDir),
            {NPos, NDir, Map#{Pos => weakened}, Count};
        weakened ->
            NDir = Dir,
            NPos = move(Pos, NDir),
            {NPos, NDir, Map#{Pos => infected}, Count + 1};
        infected ->
            NDir = turn(Dir, right),
            NPos = move(Pos, NDir),
            {NPos, NDir, Map#{Pos => flagged}, Count};
        flagged ->
            NDir = flip(Dir),
            NPos = move(Pos, NDir),
            {NPos, NDir, Map#{Pos => clean}, Count}
    end.
