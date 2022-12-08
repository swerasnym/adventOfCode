-module(aoc2022_day8).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day8.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

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

read(File) ->
    tools:read_grid(File, fun(Pos, Char) -> {Pos, Char - $0} end).

star1(Grid0) ->
    Grid1 = tools:rotate_grid(Grid0),
    Grid2 = tools:rotate_grid(Grid1),
    Grid3 = tools:rotate_grid(Grid2),

    Dir0 = tools:grid_to_lists(Grid0),
    Dir1 = tools:grid_to_lists(Grid1),
    Dir2 = tools:grid_to_lists(Grid2),
    Dir3 = tools:grid_to_lists(Grid3),

    Dirs = Dir0 ++ Dir1 ++ Dir2 ++ Dir3,

    Vis = lists:flatten([vissible(Line, -1, []) || Line <- Dirs]),
    length(lists:uniq(Vis)).

star2(Grid0) ->
    Scores = [score(P, Grid0) || P = {_, _} <- maps:keys(Grid0)],
    lists:max(Scores).

vissible([{P, H} | Rest], Max, Acc) when H > Max ->
    vissible(Rest, H, [P | Acc]);
vissible([_ | Rest], Max, Acc) ->
    vissible(Rest, Max, Acc);
vissible([], _, Acc) ->
    Acc.

distance([{_, H} | Rest], Height, Acc) when H < Height ->
    distance(Rest, Height, Acc + 1);
distance([{_, H} | _], Height, Acc) when H >= Height ->
    Acc + 1;
distance(_, _, Acc) ->
    Acc.

get_lines({PX, PY}, Grid) ->
    List = maps:to_list(Grid),

    D1 = [V || {{X, Y}, V} <- List, X == PX, Y < PY],
    D2 = [V || {{X, Y}, V} <- List, X == PX, Y > PY],
    D3 = [V || {{X, Y}, V} <- List, X < PX, Y == PY],
    D4 = [V || {{X, Y}, V} <- List, X > PX, Y == PY],
    [tools:dsort(D1), lists:sort(D2), tools:dsort(D3), lists:sort(D4)].

score(Pos, Grid) ->
    {_, H} = maps:get(Pos, Grid),

    D = [distance(Line, H, 0) || Line <- get_lines(Pos, Grid)],
    tools:product(D).
