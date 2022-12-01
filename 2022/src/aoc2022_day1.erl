-module(aoc2022_day1).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day1.txt"),
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

star1(Data) ->
    lists:max([lists:sum(X) || X <- Data]).

star2(Data) ->
    Sums = tools:dsort([lists:sum(X) || X <- Data]),
    lists:sum(lists:sublist(Sums, 3)).

read(File) ->
    tools:read_blocks(File, parse_integers).
