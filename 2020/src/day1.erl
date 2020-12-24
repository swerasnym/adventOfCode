-module(day1).

-export([run/2]).

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
    hd([Value1 * Value2
        || Value1 <- Data, Value2 <- Data, Value1 + Value2 == 2020, Value1 =< Value2]).

star2(Data) ->
    hd([Value1 * Value2 * Value3
        || Value1 <- Data,
           Value2 <- Data,
           Value3 <- Data,
           Value1 + Value2 + Value3 == 2020,
           Value1 =< Value2,
           Value2 =< Value3]).

read(File) ->
    tools:read_integers(File).
