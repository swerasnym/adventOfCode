-module(aoc2023_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day23_ex.txt", star1, 94},
        {"examples/2023/day23_ex.txt", star2, 154}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Grid, Start, End}) ->
    dfs1(Start, End, Grid, 0).

star2({Grid, Start, End}) ->
    Multiways = [{X, Y} || {X, Y} := V <- Grid, V /= $#, multiway({X, Y}, Grid)],
    Compressed = [
        {P, simplify(P, [Start, End | Multiways], 0, P, Grid)}
     || P <- [Start, End | Multiways]
    ],
    [{LastNode, Dist}] = proplists:get_value(End, Compressed),

    dfs2(Start, LastNode, maps:from_list(Compressed), 0, []) + Dist.

read(File) ->
    #{max := {Xmax, Ymax}} = Grid = tools:read_grid(File),
    {Grid, {1, 0}, {Xmax - 1, Ymax}}.

dfs1(End, End, _Grid, Steps) ->
    Steps;
dfs1(Pos, End, Grid, Steps) ->
    Next = next(Pos, Grid, fun valid/2),
    Distances = [dfs1(N, End, Grid#{Pos := $O}, Steps + 1) || N <- Next],
    lists:max([0 | Distances]).

dfs2(End, End, _Grid, Steps, _Path) ->
    Steps;
dfs2(Pos, End, Grid, Steps, Path) ->
    Next = [{N, Dist} || {N, Dist} <- maps:get(Pos, Grid), not lists:member(N, Path)],
    Distances = [dfs2(N, End, Grid, Steps + Dist, [Pos | Path]) || {N, Dist} <- Next],
    lists:max([0 | Distances]).

multiway(Pos, Grid) ->
    length(next(Pos, Grid, not_wall())) > 2.

simplify(Pos, Ends, 0, Pos, Grid) ->
    Next = next(Pos, Grid, not_wall()),
    [simplify(N, Ends, 1, Pos, Grid) || N <- Next];
simplify(Pos, Ends, Steps, Last, Grid) ->
    case lists:member(Pos, Ends) of
        true ->
            {Pos, Steps};
        false ->
            [Next] = next(Pos, Grid, not_wall()) -- [Last],
            simplify(Next, Ends, Steps + 1, Pos, Grid)
    end.

not_wall() ->
    fun(_, S) -> S /= $# end.

next(Pos, Grid, Valid) ->
    Dirs = [north, west, east, south],
    Next = [{Dir, get_neigbour(Pos, Dir)} || Dir <- Dirs],
    [NP || {Dir, NP} <- Next, Valid(Dir, maps:get(NP, Grid, $#))].

get_neigbour({X, Y}, north) -> {X, Y - 1};
get_neigbour({X, Y}, south) -> {X, Y + 1};
get_neigbour({X, Y}, east) -> {X + 1, Y};
get_neigbour({X, Y}, west) -> {X - 1, Y}.

valid(_Dir, $.) -> true;
valid(_Dir, $#) -> false;
valid(_Dir, $O) -> false;
valid(north, $^) -> true;
valid(south, $v) -> true;
valid(east, $>) -> true;
valid(west, $<) -> true;
valid(_Dir, _) -> false.
