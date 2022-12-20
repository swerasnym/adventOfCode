-module(aoc2022_day20).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day20.txt"),
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
    tools:read_integers(File).

star1(Data) ->
    result(mix(lists:enumerate(Data))).

star2(Data) ->
    result(tools:repeat(10, fun mix/1, lists:enumerate([D * 811589153 || D <- Data]))).

result(IdxMixed) ->
    {_, Mixed} = lists:unzip(IdxMixed),
    ZeroFirst = tools:rotatewhile(fun(X) -> X /= 0 end, Mixed),
    lists:sum([lists:nth(N rem length(IdxMixed) + 1, ZeroFirst) || N <- [1000, 2000, 3000]]).

mix(IdxList) ->
    tools:repeat(length(IdxList), fun mix/2, IdxList).

mix(Idx, Mixed) ->
    [Elem = {Idx, N} | Rest] = tools:rotatewhile(fun({X, _}) -> X /= Idx end, Mixed),
    [Elem | tools:rotate(N, Rest)].
