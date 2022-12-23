-module(aoc2022_day23).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day23.txt"),
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
    tools:read_grid(File).

star1(Map) ->
    Map10 = tools:repeat(10, fun step/2, Map),
    {{MinX, MaxX}, {MinY, MaxY}} = tools:minmax_grid(Map10),
    (MaxX - MinX + 1) * (MaxY - MinY + 1) - tools:count($#, Map10).

star2(Map) ->
    same(0, Map).

same(N, Map) ->
    MapNp1 = step(N + 1, Map),
    case Map == MapNp1 of
        true ->
            tools:print_grid(MapNp1),
            N + 1;
        false ->
            same(N + 1, MapNp1)
    end.

step(N, Map) ->
    Prop = lists:sort([{decide((N - 1) rem 4, P, Map), P} || {P, $#} <- maps:to_list(Map)]),
    Move = avoid_colitions(Prop, []),
    maps:from_keys(Move, $#).

avoid_colitions([{P, A}, {P, B}, {P, C}, {P, D} | Rest], Acc) ->
    avoid_colitions(Rest, [A, B, C, D | Acc]);
avoid_colitions([{P, A}, {P, B}, {P, C} | Rest], Acc) ->
    avoid_colitions(Rest, [A, B, C | Acc]);
avoid_colitions([{P, A}, {P, B} | Rest], Acc) ->
    avoid_colitions(Rest, [A, B | Acc]);
avoid_colitions([{P, _} | Rest], Acc) ->
    avoid_colitions(Rest, [P | Acc]);
avoid_colitions([], Acc) ->
    Acc.

decide(Step, P = {X, Y}, Map) ->
    Around =
        [NW, N, NE, SW, S, SE, W, E] =
            [maps:get({Nx, Ny}, Map, $.)
             || {Nx, Ny}
                    <- [{X - 1, Y - 1},
                        {X, Y - 1},
                        {X + 1, Y - 1},
                        {X - 1, Y + 1},
                        {X, Y + 1},
                        {X + 1, Y + 1},
                        {X - 1, Y},
                        {X + 1, Y}]],

    case tools:count($#, Around) of
        0 ->
            P;
        K when K > 5 ->
            P;
        _ ->
            case {tools:count($#, [N, NW, NE]),
                  tools:count($#, [S, SW, SE]),
                  tools:count($#, [W, NW, SW]),
                  tools:count($#, [E, NE, SE])}
            of
                {0, _, _, _} when Step == 0 ->
                    {X, Y - 1};
                {_, 0, _, _} when Step == 0 ->
                    {X, Y + 1};
                {_, _, 0, _} when Step == 0 ->
                    {X - 1, Y};
                {_, _, _, 0} when Step == 0 ->
                    {X + 1, Y};
                %%
                {_, 0, _, _} when Step == 1 ->
                    {X, Y + 1};
                {_, _, 0, _} when Step == 1 ->
                    {X - 1, Y};
                {_, _, _, 0} when Step == 1 ->
                    {X + 1, Y};
                {0, _, _, _} when Step == 1 ->
                    {X, Y - 1};
                %%
                {_, _, 0, _} when Step == 2 ->
                    {X - 1, Y};
                {_, _, _, 0} when Step == 2 ->
                    {X + 1, Y};
                {0, _, _, _} when Step == 2 ->
                    {X, Y - 1};
                {_, 0, _, _} when Step == 2 ->
                    {X, Y + 1};
                %%
                {_, _, _, 0} when Step == 3 ->
                    {X + 1, Y};
                {0, _, _, _} when Step == 3 ->
                    {X, Y - 1};
                {_, 0, _, _} when Step == 3 ->
                    {X, Y + 1};
                {_, _, 0, _} when Step == 3 ->
                    {X - 1, Y};
                _ ->
                    P
            end
    end.
