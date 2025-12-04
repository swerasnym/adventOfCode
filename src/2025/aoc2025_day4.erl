-module(aoc2025_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day4_ex.txt", star1, 13},
        {"examples/2025/day4_ex.txt", star2, 43}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    Rolls = [Pos || Pos := $@ <- Map],
    length([R || R <- Rolls, movable(R, Map)]).

star2(Map) ->
    remove_all(Map, 0).

read(File) ->
    tools:read_grid(File).

neighbours({X, Y}) ->
    [{X + Dx, Y + Dy} || Dx <- [-1, 0, 1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}].

movable(Pos, Map) ->
    Neighbours = [maps:get(N, Map, $.) || N <- neighbours(Pos)],
    tools:count($@, Neighbours) < 4.

remove_all(Map, Total) ->
    Rolls = [Pos || Pos := $@ <- Map],
    Movable = [R || R <- Rolls, movable(R, Map)],
    case length(Movable) of
        0 ->
            tools:print_grid(Map),
            Total;
        N ->
            Moved = maps:from_keys(Movable, $.),
            Next = maps:merge(Map, Moved),
            remove_all(Next, Total + N)
    end.
