-module(aoc2023_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day16_ex.txt", star1, 46},
        {"examples/2023/day16_ex.txt", star2, 51}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    light([{{0, 0}, east, 1}], Grid, #{}).

star2(Grid) ->
    lists:max([light([{Spos, Sdir, 1}], Grid, #{}) || {Spos, Sdir} <- starts(Grid)]).

read(File) ->
    tools:read_grid(File).

light([], _Grid, Lighted) ->
    length(lists:usort([Pos || {Pos, _} <- maps:keys(Lighted)]));
light([{Pos, Dir, N} | Rest], Grid, Lighted) ->
    case maps:get({Pos, Dir}, Lighted, first) of
        first ->
            Symbol = maps:get(Pos, Grid, $#),
            case Symbol of
                $# ->
                    light(Rest, Grid, Lighted);
                _ ->
                    NewDirs = interact(Dir, Symbol),
                    Next = [{get_neighbour(Pos, Nd), Nd, N + 1} || Nd <- NewDirs],
                    light(Rest ++ Next, Grid, Lighted#{{Pos, Dir} => N})
            end;
        _ ->
            light(Rest, Grid, Lighted)
    end.

get_neighbour({X, Y}, north) -> {X, Y - 1};
get_neighbour({X, Y}, south) -> {X, Y + 1};
get_neighbour({X, Y}, east) -> {X + 1, Y};
get_neighbour({X, Y}, west) -> {X - 1, Y}.

interact(Dir, $.) ->
    [Dir];
interact(Dir, $-) ->
    case Dir of
        east -> [east];
        west -> [west];
        _ -> [east, west]
    end;
interact(Dir, $|) ->
    case Dir of
        north -> [north];
        south -> [south];
        _ -> [north, south]
    end;
interact(Dir, $/) ->
    case Dir of
        north -> [east];
        south -> [west];
        east -> [north];
        west -> [south]
    end;
interact(Dir, $\\) ->
    case Dir of
        north -> [west];
        south -> [east];
        east -> [south];
        west -> [north]
    end;
interact(_Dir, $#) ->
    [].

starts(#{max := {Xmax, Ymax}}) ->
    [{{0, Y}, east} || Y <- lists:seq(0, Ymax)] ++
        [{{Xmax, Y}, west} || Y <- lists:seq(0, Ymax)] ++
        [{{X, 0}, south} || X <- lists:seq(0, Xmax)] ++
        [{{X, Ymax}, north} || X <- lists:seq(0, Xmax)].
