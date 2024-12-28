-module(aoc2017_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {{data, [3, 4, 1, 5]}, {star1, 4}, 12},
        {{data, ""}, star2, "a2582a3a0e66e6e86e3812dcb672a272"},
        {{data, "AoC 2017"}, star2, "33efeb34ea91902bb2f59c9920caa6cd"},
        {{data, "1,2,3"}, star2, "3efbe78a8d82f29979031a4aa0b16a9d"},
        {{data, "1,2,4"}, star2, "63960835bcdc130f0b66d7ff4f6a5a8e"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(String) ->
    star1(tools:parse_integers(String, ","), 255).
star1(Lengths, Max) ->
    [A, B | _] = fold(Lengths, Max),
    A * B.
star2(Lengths) ->
    Sequence = Lengths ++ [17, 31, 73, 47, 23],
    FullSequence = lists:flatten(lists:duplicate(64, Sequence)),
    output(fold(FullSequence, 255), 0).

read(File) ->
    tools:read_string(File).

fold(Lengths, Max) ->
    {_, _, End} = lists:foldl(fun twist/2, {0, 0, lists:seq(0, Max)}, Lengths),
    End.

twist(L, {Start, Skip, List}) ->
    Rotated = tools:rotate(Start, List),
    {First, Rest} = lists:split(L, Rotated),
    {Start + L + Skip, Skip + 1, tools:rotate(-Start, lists:reverse(First, Rest))}.

output([], Sum) ->
    string:to_lower(integer_to_list(Sum, 16));
output(Stream, Sum) ->
    {Block, Rest} = lists:split(16, Stream),
    Compact = lists:foldl(fun erlang:'bxor'/2, 0, Block),
    output(Rest, Sum * 256 + Compact).
