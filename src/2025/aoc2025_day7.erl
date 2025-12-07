-module(aoc2025_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day7_ex.txt", star1, 21},
        {"examples/2025/day7_ex.txt", star2, 40}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({S, Grid}) ->
    simulate(Grid, [S], 0).

star2({S, Grid}) ->
    simulate2(Grid, [S], #{S => 1}).

read(File) ->
    Grid = tools:read_grid(File),
    [Start] = [S || S := $S <- Grid],
    {Start, Grid}.

down({X, Y}) -> {X, Y + 1}.

split({X, Y}) -> [{X - 1, Y}, {X + 1, Y}].

simulate(Grid, [Pos | Rest], Splits) ->
    Down = down(Pos),
    case maps:get(Down, Grid, bottom) of
        bottom ->
            tools:print_grid(Grid),
            Splits;
        $| ->
            simulate(Grid, Rest, Splits);
        $. ->
            simulate(Grid#{Down => $|}, Rest ++ [Down], Splits);
        $^ ->
            Split = [L, R] = split(Down),
            simulate(Grid#{L => $|, R => $|}, Rest ++ Split, Splits + 1)
    end.

simulate2(Grid, [Pos | Rest], Counts) ->
    Down = down(Pos),
    Beams = maps:get(Pos, Counts),
    Split = [L, R] = split(Down),
    case maps:get(Down, Grid, bottom) of
        bottom ->
            lists:sum([maps:get(P, Counts) || P <- [Pos | Rest]]);
        $| ->
            BeamsOut = Beams + maps:get(Down, Counts),
            case [maps:get(L, Grid), maps:get(R, Grid)] of
                [$^, _] -> simulate2(Grid, Rest, Counts#{Down => BeamsOut});
                [_, $^] -> simulate2(Grid, Rest, Counts#{Down => BeamsOut});
                [_, _] -> simulate2(Grid, Rest, Counts#{Down => Beams})
            end;
        $. ->
            simulate2(Grid#{Down => $|}, Rest ++ [Down], Counts#{Down => Beams});
        $^ ->
            BeamsOutL = Beams + maps:get(L, Counts, 0),
            BeamsOutR = Beams + maps:get(R, Counts, 0),
            simulate2(Grid#{L => $|, R => $|}, Rest ++ Split, Counts#{
                L => BeamsOutL, R => BeamsOutR
            })
    end.
