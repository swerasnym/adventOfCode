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
    {outside, Path} = guard_path(StartPos, north, Map#{StartPos => $.}, [], #{}),
    {Positions, _} = lists:unzip(Path),
    length(lists:usort(Positions)).

star2(Map) ->
    [StartPos] = [Pos || Pos := $^ <- Map],
    {outside, Path} = guard_path(StartPos, north, Map#{StartPos => $.}, [], #{}),
    count_loops(Path, Map#{StartPos => $.}).

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

guard_path(Pos, Dir, Map, Path, Visited) ->
    Next = move(Pos, Dir),
    PosDir = {Pos, Dir},
    BeenHere = maps:get(PosDir, Visited, false),
    case {BeenHere, maps:get(Next, Map, outside)} of
        {true, _} ->
            {loop, lists:reverse([PosDir | Path])};
        {false, outside} ->
            {outside, lists:reverse([PosDir | Path])};
        {false, $.} ->
            guard_path(Next, Dir, Map, [PosDir | Path], Visited#{PosDir => true});
        {false, $#} ->
            guard_path(Pos, get_turn(Dir), Map, Path, Visited#{PosDir => true})
    end.

count_loops(Path, Map) ->
    count_loops(Path, Map, 0, #{}, #{}).

count_loops([_], _, Loops, _, _) ->
    Loops;
count_loops([{Pos, Dir} | Path], Map, Loops, Visited0, Checked) ->
    {Block, _} = hd(Path),
    Visited = Visited0#{{Pos, Dir} => true},
    case maps:get(Block, Checked, false) of
        true ->
            count_loops(Path, Map, Loops, Visited, Checked);
        false ->
            {Exit, _} = guard_path(Pos, Dir, Map#{Block => $#}, [], Visited0),
            LoopsNext = tools:inc_on_true(Exit == loop, Loops),
            count_loops(Path, Map, LoopsNext, Visited, Checked#{Block => true})
    end.
