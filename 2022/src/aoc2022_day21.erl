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
    {M1, _, Wanted} = yell2("root", 0, Data),
    {M1, _, Wanted} = yell2("root", 1, Data),
    search(0, limits(Wanted, M1, Data), Wanted, M1, Data).

parse_monkey(Line) ->
    [Monkey, Job] = string:split(Line, ": "),
    case string:split(Job, " ", all) of
        [M1, "/", M2] ->
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

yell2("root", Humn, Map) ->
    {M1, _Op, M2} = maps:get("root", Map),
    {M1, yell2(M1, Humn, Map), yell2(M2, Humn, Map)};
yell2("humn", Humn, _Map) ->
    Humn;
yell2(M, Humn, Map) ->
    case maps:get(M, Map) of
        {D} ->
            D;
        {M1, Op, M2} ->
            apply(erlang, Op, [yell2(M1, Humn, Map), yell2(M2, Humn, Map)])
    end.

limits(Wanted, M1, Map) ->
    true = yell2(M1, 0, Map) > Wanted,
    true = yell2(M1, 0, Map) > yell2(M1, 1, Map),
    find_upper(M1, 1, Map, Wanted).

find_upper(M1, N, Map, Wanted) ->
    case yell2(M1, N, Map) > Wanted of
        true ->
            find_upper(M1, N * 2, Map, Wanted);
        false ->
            N
    end.

search(N, N, Wanted, M1, Map) ->
    Wanted = yell2(M1, N, Map);
search(Lower, Upper, Wanted, M1, Map) ->
    Mid = (Lower + Upper) div 2,

    case yell2(M1, Mid, Map) - Wanted of
        0 ->
            Mid;
        N when N > 0 ->
            search(Mid, Upper, Wanted, M1, Map);
        _ ->
            search(Lower, Mid, Wanted, M1, Map)
    end.
