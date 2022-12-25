-module(aoc2022_day25).

-export([run/0, run/2, integer_to_snafu/1, snafu_to_integer/1]).

run() ->
    {S1, S2} = Res = run(all, "../data/day25.txt"),
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
    tools:read_lines(File, fun snafu_to_integer/1).

star1(Data) ->
    integer_to_snafu(lists:sum(Data)).

star2(_) ->
    "2022 Done!".

snafu_to_integer(Str) ->
    snafu_to_integer(Str, 0).

snafu_to_integer([], Acc) ->
    Acc;
snafu_to_integer("-" ++ Rest, Acc) ->
    snafu_to_integer(Rest, 5 * Acc - 1);
snafu_to_integer("=" ++ Rest, Acc) ->
    snafu_to_integer(Rest, 5 * Acc - 2);
snafu_to_integer([D | Rest], Acc) ->
    snafu_to_integer(Rest, 5 * Acc + D - $0).

integer_to_snafu(0) ->
    "0";
integer_to_snafu(N) ->
    integer_to_snafu(N, []).

integer_to_snafu(0, Acc) ->
    Acc;
integer_to_snafu(N, Acc) ->
    case N rem 5 of
        -4 ->
            integer_to_snafu(N div 5 - 1, [$1 | Acc]);
        -3 ->
            integer_to_snafu(N div 5 - 1, [$2 | Acc]);
        -2 ->
            integer_to_snafu(N div 5, [$= | Acc]);
        -1 ->
            integer_to_snafu(N div 5, [$- | Acc]);
        0 ->
            integer_to_snafu(N div 5, [$0 | Acc]);
        1 ->
            integer_to_snafu(N div 5, [$1 | Acc]);
        2 ->
            integer_to_snafu(N div 5, [$2 | Acc]);
        3 ->
            integer_to_snafu(N div 5 + 1, [$= | Acc]);
        4 ->
            integer_to_snafu(N div 5 + 1, [$- | Acc])
    end.
