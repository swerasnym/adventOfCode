-module(aoc2021_day1).

-export([run/2, larger/2, window/2]).

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
    larger(Data, 0).

star2(Data) ->
    larger(window(Data, []), 0).

read(File) ->
    tools:read_integers(File).

larger([], Result) ->
    Result;
larger([A, B | Rest], Result) when A < B ->
    larger([B | Rest], Result + 1);
larger([_ | Rest], Result) ->
    larger(Rest, Result).

window([A | [B, C | _] = Rest], Result) ->
    window(Rest, [A + B + C | Result]);
window(_, Result) ->
    lists:reverse(Result).
