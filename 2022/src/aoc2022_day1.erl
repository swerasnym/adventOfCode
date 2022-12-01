-module(aoc2022_day1).


-export([run/0, run/2]).
run() ->
    {S1, S2} = Res =  run(all, "../data/day1.txt"),
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
    {Top3, _} = lists:split(3, lists:reverse( lists:sort([lists:sum(X) || X <- Data]))),
    lists:sum(Top3).


 

read(File) ->
    Blocks = tools:read_blocks(File),
    [tools:parse_integers(Block) || Block <- Blocks].


