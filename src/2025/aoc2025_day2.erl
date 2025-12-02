-module(aoc2025_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day2_ex.txt", star1, 1227775554},
        {"examples/2025/day2_ex.txt", star2, 4174379265}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Max, Ranges}) ->
    Repeating = repeating(1, Max, []),
    Invalid = [I || I <- Repeating, R <- Ranges, in_range(R, I)],
    lists:sum(Invalid).

star2({Max, Ranges}) ->
    Repeating = repeating2(1, Max, [], 2),
    Invalid = [I || I <- Repeating, R <- Ranges, in_range(R, I)],
    lists:sum(Invalid).

read(File) ->
    Integers = tools:read_integers(File, ",-\n"),
    {lists:max(Integers), tools:group(2, Integers)}.

repeating(Half, Max, Acc) ->
    H = integer_to_list(Half),
    Value = list_to_integer(H ++ H),
    case Value > Max of
        true -> Acc;
        false -> repeating(Half + 1, Max, [Value | Acc])
    end.

in_range({Min, Max}, Value) -> Min =< Value andalso Max >= Value.

repeating2(Part, Max, Acc, Times) ->
    P = integer_to_list(Part),
    S = lists:flatten(lists:duplicate(Times, P)),
    Value = list_to_integer(S),
    case Value > Max of
        true when Times == 2 -> lists:usort(Acc);
        true -> repeating2(Part + 1, Max, [Value | Acc], 2);
        false -> repeating2(Part, Max, [Value | Acc], Times + 1)
    end.
