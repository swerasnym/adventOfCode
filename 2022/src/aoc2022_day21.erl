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

star2(_Data) ->
    ok.

parse_monkey(Line) ->
    [Monkey, Job] = string:split(Line, ": "),
    case string:split(Job, " ", all) of
        [M1, "/", M2] ->
            {Monkey, {M1, 'div', M2}};
        [M1, Op, M2] ->
            {Monkey, {M1, list_to_existing_atom(Op), M2}};
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
