-module(aoc2025_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day5_ex.txt", star1, 3},
        {"examples/2025/day5_ex.txt", star2, 14},
        {"examples/2025/day5_ex2.txt", star1, 3},
        {"examples/2025/day5_ex2.txt", star2, 1006}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Ranges, Values}) ->
    Fresh = [V || V <- Values, in_any_range(Ranges, V)],
    length(Fresh).

star2({Ranges, _Values}) ->
    SortedRanges = lists:sort(Ranges),
    Merged = merge_intervals(SortedRanges, []),
    Items = [items(I) || I <- Merged],
    lists:sum(Items).

read(File) ->
    [RangesS, ValuesS] = tools:read_blocks(File),
    Values = tools:parse_integers(ValuesS),
    Ranges = tools:group(2, tools:parse_integers(RangesS, "-\n,")),
    {Ranges, Values}.

in_any_range([], _) ->
    false;
in_any_range([R | Range], Value) ->
    case in_range(R, Value) of
        true -> true;
        false -> in_any_range(Range, Value)
    end.

in_range({Min, Max}, Value) -> Min =< Value andalso Max >= Value.

merge_intervals([A], Acc) ->
    [A | Acc];
merge_intervals([{MinA, MaxA} = A, {_, MaxB} = B | Rest], Acc) ->
    case overlap(A, B) of
        true ->
            merge_intervals([{MinA, max(MaxA, MaxB)} | Rest], Acc);
        false ->
            merge_intervals([B | Rest], [A | Acc])
    end.

overlap({A1, A2}, {B1, B2}) -> A1 =< B2 andalso B1 =< A2.

items({Min, Max}) -> Max - Min + 1.
