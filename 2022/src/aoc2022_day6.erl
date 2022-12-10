-module(aoc2022_day6).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day6.txt"),
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
    [Data] = tools:read_lines(File),
    Data.

star1(Data) ->
    start(4, Data).

star2(Data) ->
    start_m(14, Data).

start(Pos, [A | Rest = [B, C, D | _]]) ->
    case length(lists:uniq([A, B, C, D])) of
        4 ->
            Pos;
        _ ->
            start(Pos + 1, Rest)
    end.

start_m(Pos, L = [_ | Rest]) ->
    case length(lists:uniq(lists:sublist(L, 14))) of
        14 ->
            Pos;
        _ ->
            start_m(Pos + 1, Rest)
    end.
