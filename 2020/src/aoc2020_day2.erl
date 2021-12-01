-module(aoc2020_day2).

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
    length([Line || Line <- Data, check_pwd1(Line)]).

star2(Data) ->
    length([Line || Line <- Data, check_pwd2(Line)]).

read(File) ->
    tools:read_format(File, "~d-~d ~c:~s").

check_pwd1([Min, Max, [Char], Password]) ->
    Len = length([P || P <- Password, P == Char]),
    Len >= Min andalso Len =< Max.

check_pwd2([Idx1, Idx2, [Char], Password]) ->
    Pos1 = lists:nth(Idx1, Password),
    Pos2 = lists:nth(Idx2, Password),
    (Pos1 == Char) xor (Pos2 == Char).
