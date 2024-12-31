-module(aoc2017_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day19_ex.txt", star1, "ABCDEF"},
        {"examples/2017/day19_ex.txt", star2, 38}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 19},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Map, Start}) ->
    {Path, _} = follow(Start, Map, [], 0),
    Path.

star2({Map, Start}) ->
    {_, Steps} = follow(Start, Map, [], 0),
    Steps.

read(File) ->
    Map = tools:read_grid(File),
    Start = [{Pos, {0, 1}} || {_, 0} = Pos := $| <- Map],
    {Map, Start}.

follow([], _, Acc, Steps) ->
    {lists:reverse(Acc), Steps};
follow([{Pos, Dir} | Rest], Map, Acc, Steps) ->
    case maps:get(Pos, Map, $\s) of
        $\s ->
            follow(Rest, Map, Acc, Steps);
        $+ ->
            {Dx, Dy} = Dir,
            NewDir = [{-Dy, -Dx}, {Dy, Dx}],
            New = [{aoc_vector:add(Pos, D), D} || D <- NewDir],
            follow(Rest ++ New, Map, Acc, Steps + 1);
        $- ->
            follow(Rest ++ [{aoc_vector:add(Pos, Dir), Dir}], Map, Acc, Steps + 1);
        $| ->
            follow(Rest ++ [{aoc_vector:add(Pos, Dir), Dir}], Map, Acc, Steps + 1);
        S ->
            follow(Rest ++ [{aoc_vector:add(Pos, Dir), Dir}], Map, [S | Acc], Steps + 1)
    end.
