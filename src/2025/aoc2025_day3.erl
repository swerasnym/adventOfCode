-module(aoc2025_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2025/day3_ex.txt", star1, 357},
        {"examples/2025/day3_ex.txt", {star2, 2}, 357},
        {"examples/2025/day3_ex.txt", star2, 3121910778619}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Lines) ->
    Pairs = [max_ordered_pair(L) || L <- Lines],
    lists:sum(Pairs).

star2(Lines) ->
    star2(Lines, 12).

star2(Lines, N) ->
    Pairs = [max_ordered_n(Line, N) || Line <- Lines],
    lists:sum(Pairs).

read(File) ->
    tools:read_lines(File).

max_ordered_pair(Line) ->
    Rev = lists:reverse(Line),
    [_ | Tail] = [{V, P} || {P, V} <- lists:enumerate(Rev)],
    {Tv, Tp} = lists:max(Tail),

    Ov = lists:max(lists:sublist(Rev, Tp - 1)),
    % io:format("~c~c~n", [Tv, Ov]),
    list_to_integer([Tv, Ov]).

max_ordered_n(Line, N) ->
    Rev = lists:reverse(Line),
    Enum = [{V, P} || {P, V} <- lists:enumerate(0, Rev)],
    max_ordered_n(Enum, N, []).

max_ordered_n(_, 0, Acc) ->
    list_to_integer(lists:reverse(Acc));
max_ordered_n(Enum, N, Acc) ->
    Tail = lists:nthtail(N - 1, Enum),
    {Tv, Tp} = lists:max(Tail),
    max_ordered_n(lists:sublist(Enum, Tp), N - 1, [Tv | Acc]).
