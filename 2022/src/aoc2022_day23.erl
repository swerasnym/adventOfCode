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
    same(1, Map).

same(N, Map) ->
    Next = step(N, Map),
    case Map == Next of
        true ->
            tools:print_grid(Next),
            N;
        false ->
            same(N + 1, Next)
    end.

step(N, Map) ->
    Prop = lists:sort([{decide(N, P, Map), P} || {P, $#} <- maps:to_list(Map)]),
    Move = avoid_colitions(Prop, []),
    maps:from_keys(Move, $#).

avoid_colitions([{P, A}, {P, B} | Rest], Acc) ->
    avoid_colitions(Rest, [A, B | Acc]);
avoid_colitions([{P, _} | Rest], Acc) ->
    avoid_colitions(Rest, [P | Acc]);
avoid_colitions([], Acc) ->
    Acc.

decide(Step, P = {X, Y}, Map) ->
    case [maps:get(N, Map, $.) || N <- neigbours(P)] of
        "........" ->
            P;
        [NW, N, NE, W, E, SW, S, SE] ->
            move(P,
                 tools:rotate(Step,
                              [{[E, NE, SE], {X + 1, Y}},
                               {[N, NW, NE], {X, Y - 1}},
                               {[S, SW, SE], {X, Y + 1}},
                               {[W, NW, SW], {X - 1, Y}}]))
    end.

move(P, []) ->
    P;
move(_P, [{"...", To} | _]) ->
    To;
move(P, L) ->
    move(P, tl(L)).

neigbours({X, Y}) ->
    [{X - 1, Y - 1},
     {X, Y - 1},
     {X + 1, Y - 1},
     {X - 1, Y},
     {X + 1, Y},
     {X - 1, Y + 1},
     {X, Y + 1},
     {X + 1, Y + 1}].
