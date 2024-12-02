-module(aoc2024_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day2_ex.txt", star1, 2},
        {"examples/2024/day2_ex.txt", star2, 4}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Safe = [S || S <- Data, is_safe(S)],
    length(Safe).

star2(Data) ->
    Safe = [S || S <- Data, is_safe_dampened([], S)],
    length(Safe).

read(File) ->
    tools:read_lines(File, fun tools:parse_integers/1).

is_safe(Line) ->
    Diffs = diffs(Line, []),
    Min = lists:min(Diffs),
    Max = lists:max(Diffs),
    is_monotonic(Line) andalso Min > 0 andalso Max < 4.

is_monotonic(Line) ->
    Sorted = lists:sort(Line),
    Line == Sorted orelse lists:reverse(Line) == Sorted.

diffs([_], Acc) ->
    Acc;
diffs([A | [B | _] = Rest], Acc) ->
    diffs(Rest, [abs(A - B) | Acc]).

is_safe_dampened(_Head, []) ->
    false;
is_safe_dampened(Head, [A | Rest]) ->
    case is_safe(Head ++ Rest) of
        true ->
            true;
        false ->
            is_safe_dampened(Head ++ [A], Rest)
    end.
