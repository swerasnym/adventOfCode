-module(aoc2022_day25).
-behaviour(aoc_solution).

-export([run/0, run/2, integer_to_snafu/1, snafu_to_integer/1]).
%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 25}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_lines(File, fun snafu_to_integer/1).

star1(Data) ->
    integer_to_snafu(lists:sum(Data)).

star2(_) ->
    {done, "2022 Done!"}.

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
