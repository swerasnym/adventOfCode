-module(aoc2021_day13).

-export([run/2, profile/3]).

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

profile(Star, File, Times) ->
    Data = read(File),

    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

star1({Points, Folds}) ->
    Fold1 = lists:map(fold(hd(Folds)), Points),
    Sort = lists:usort(Fold1),
    length(Sort).

star2({Points, Folds}) ->
    Sort = folds(Points, Folds),
    Grid = maps:from_list([{P, $█} || P <- Sort]),
    tools:print_grid(Grid).

read(File) ->
    [Points, Instructions] = tools:read_blocks(File),

    {P, I} =
        {tools:parse_format(Points, "~d,~d"),
         tools:parse_format(Instructions, "fold along ~c=~d\n")},
    {[{X, Y} || [X, Y] <- P], I}.

fold(["x", Xline]) ->
    fun ({X, Y}) when X > Xline ->
            {2 * Xline - X, Y};
        (Point) ->
            Point
    end;
fold(["y", Yline]) ->
    fun ({X, Y}) when Y > Yline ->
            {X, 2 * Yline - Y};
        (Point) ->
            Point
    end.

folds(Points, []) ->
    Points;
folds(Points, [I | Is]) ->
    Fold = lists:map(fold(I), Points),
    folds(lists:usort(Fold), Is).
