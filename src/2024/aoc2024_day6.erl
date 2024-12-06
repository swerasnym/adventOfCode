-module(aoc2024_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day6_ex.txt", star1, 41},
        {"examples/2024/day6_ex.txt", star2, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    [StartPos] = [Pos || Pos := $^ <- Map],
    Path = move(StartPos, north, Map#{StartPos => $.}, [], #{}),
    length(lists:usort(Path)).

star2(Map) ->
    [StartPos] = [Pos || Pos := $^ <- Map],
    Path = move(StartPos, north, Map#{StartPos => $.}, [], #{}),
    Loops = [Obstruction || Obstruction <- lists:usort(Path), is_loop(StartPos, Map, Obstruction)],
    length(Loops).

read(File) ->
    tools:read_grid(File).

move({X, Y}, north) ->
    {X, Y - 1};
move({X, Y}, south) ->
    {X, Y + 1};
move({X, Y}, east) ->
    {X + 1, Y};
move({X, Y}, west) ->
    {X - 1, Y}.

get_turn(north) ->
    east;
get_turn(south) ->
    west;
get_turn(east) ->
    south;
get_turn(west) ->
    north.
is_loop(StartPos, _Map, StartPos) ->
    false;
is_loop(StartPos, Map, Obstruction) ->
    case move(StartPos, north, Map#{Obstruction => $#, StartPos => $.}, [], #{}) of
        {loop, _} ->
            true;
        _ ->
            false
    end.

move(Pos, Dir, Map, Path, Visited) ->
    Next = move(Pos, Dir),
    BeenHere = maps:get({Pos, Dir}, Visited, false),
    case {BeenHere, maps:get(Next, Map, outside)} of
        {true, _} ->
            {loop, [Pos | Path]};
        {false, outside} ->
            [Pos | Path];
        {false, $.} ->
            move(Next, Dir, Map, [Pos | Path], Visited#{{Pos, Dir} => true});
        {false, $#} ->
            move(Pos, get_turn(Dir), Map, Path, Visited#{{Pos, Dir} => true})
    end.
