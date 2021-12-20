-module(aoc2021_day20).

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

star1({Line, #{max := {Mx, My}} = Grid}) ->
    {Grid1, Out1} = enhance(Grid, Line, dark),

    {Grid2, Out2} = enhance(Grid1, Line, Out1),
    print(Grid2),
    tools:count(light, Grid2).

star2({Line, Grid}) ->
    Grid50 = enhance(Grid, Line, dark, 50),
    print(Grid50),
    tools:count(light, Grid50).

read(File) ->
    Replacements = #{$# => light, $. => dark},

    [Line, Grid] = tools:read_blocks(File),
    {tools:replace(Line, Replacements), tools:parse_grid(Grid, Replacements)}.

print(Grid) ->
    tools:print_grid(
        tools:replace(Grid, #{light => $█, dark => $ })).

neigbours({X, Y} = Pos) ->
    [{X + Dx, Y + Dy} || Dy <- lists:seq(-1, 1), Dx <- lists:seq(-1, 1)].

index(Pos, Grid, Line, Outside) ->
    Sequence = [maps:get(P, Grid, Outside) || P <- neigbours(Pos)],
    Index = list_to_integer(tools:replace(Sequence, #{light => $1, dark => $0}), 2),
    {Pos, lists:nth(Index + 1, Line)}.

enhance(Grid, Line, Outside, 0) ->
    Grid;
enhance(Grid, Line, Outside, N) ->
    io:format("~p: ~p ~p~n", [N, tools:minmax_grid(Grid), Outside]),
    {Grid1, Out1} = enhance(Grid, Line, Outside),
    enhance(Grid1, Line, Out1, N - 1).

enhance(Grid, Line, Outside) ->
    {{Xmin, Xmax}, {Ymin, Ymax}} = tools:minmax_grid(Grid),

    Processed =
        [index({X, Y}, Grid, Line, Outside)
         || X <- lists:seq(Xmin - 1, Xmax + 1), Y <- lists:seq(Ymin - 1, Ymax + 1)],
    Out = maps:from_list(Processed),
    Os = case Outside of
             dark ->
                 lists:nth(1, Line);
             light ->
                 lists:nth(512, Line)
         end,
    {Out, Os}.
