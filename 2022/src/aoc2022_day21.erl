-module(aoc2022_day21).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day21.txt"),
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
    maps:from_list(tools:read_lines(File, fun parse_monkey/1)).

star1(Data) ->
    yell("root", Data).

star2(Data) ->
    search(Data).

parse_monkey(Line) ->
    [Monkey, Job] = string:split(Line, ": "),
    case string:split(Job, " ", all) of
        [M1, "/", M2] ->
            %% assumption for the right results we get nice integers
            {Monkey, {M1, 'div', M2}};
        [M1, "+", M2] ->
            {Monkey, {M1, '+', M2}};
        [M1, "-", M2] ->
            {Monkey, {M1, '-', M2}};
        [M1, "*", M2] ->
            {Monkey, {M1, '*', M2}};
        [D] ->
            {Monkey, {list_to_integer(D)}}
    end.

yell(M, Map) ->
    case maps:get(M, Map) of
        {D} ->
            D;
        {M1, Op, M2} ->
            apply(erlang, Op, [yell(M1, Map), yell(M2, Map)])
    end.

yell2(Humn, Map) ->
    {M1, _Op, M2} = maps:get("root", Map),
    yell2(M1, Humn, Map) - yell2(M2, Humn, Map).

yell2("humn", Humn, _Map) ->
    Humn;
yell2(M, Humn, Map) ->
    case maps:get(M, Map) of
        {D} ->
            D;
        {M1, Op, M2} ->
            apply(erlang, Op, [yell2(M1, Humn, Map), yell2(M2, Humn, Map)])
    end.

search(Map) ->
    %% Assumption the function is linear wrt humn.
    Neg = find(fun(X) -> X < 0 end, 1, Map),
    Pos = find(fun(X) -> X > 0 end, 1, Map),
    search(Neg, Pos, Map, tools:sign(Pos - Neg)).

search(N, N, _, _) ->
    N;
search(Neg, Pos, Map, PM1) ->
    Mid = (Neg + Pos) div 2,
    case yell2(Mid, Map) of
        0 ->
            Mid;
        N when N > 0 ->
            search(Neg, Mid + PM1, Map, PM1);
        N when N < 0 ->
            search(Mid, Pos - PM1, Map, PM1)
    end.

find(_, N, _) when N > 1 bsl 64 ->
    %% Asume 64 bits are enough
    error(out_of_range);
find(F, N, Map) ->
    case {F(yell2(N, Map)), F(yell2(-N, Map))} of
        {true, _} ->
            N;
        {_, true} ->
            -N;
        _ ->
            find(F, 2 * N, Map)
    end.
