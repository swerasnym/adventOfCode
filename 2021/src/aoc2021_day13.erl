-module(aoc2021_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 13},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Points, Folds}) ->
    Fold1 = lists:map(fold(hd(Folds)), Points),
    Sort = lists:usort(Fold1),
    length(Sort).

star2({Points, Folds}) ->
    Sort = folds(Points, Folds),
    Grid = maps:from_list([{P, $█} || P <- Sort]),
    tools:print_grid(Grid),
    manual.

read(File) ->
    [Points, Instructions] = tools:read_blocks(File),

    {P, I} =
        {
            tools:parse_format(Points, "~d,~d"),
            tools:parse_format(Instructions, "fold along ~c=~d\n")
        },
    {[{X, Y} || [X, Y] <- P], I}.

fold(["x", Xline]) ->
    fun
        ({X, Y}) when X > Xline ->
            {2 * Xline - X, Y};
        (Point) ->
            Point
    end;
fold(["y", Yline]) ->
    fun
        ({X, Y}) when Y > Yline ->
            {X, 2 * Yline - Y};
        (Point) ->
            Point
    end.

folds(Points, []) ->
    Points;
folds(Points, [I | Is]) ->
    Fold = lists:map(fold(I), Points),
    folds(lists:usort(Fold), Is).
