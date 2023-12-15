-module(aoc2023_day01).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 1}}).

run() ->
    %% aoc_solution:run(?MODULE, all, "2023/data/day1_ex.txt").
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:sum([number(filter1(V, [])) || V <- Data]).

star2(Data) ->
    lists:sum([number(filter2(V, [])) || V <- Data]).

read(File) ->
    tools:read_lines(File).

filter1([], V) ->
    lists:reverse(V);
filter1([N | Rest], V) when N >= $0, N =< $9 ->
    filter1(Rest, [N | V]);
filter1([_ | Rest], V) ->
    filter1(Rest, V).

number([]) ->
    0;
number(D) ->
    list_to_integer([hd(D), lists:last(D)]).

filter2([], V) ->
    lists:reverse(V);
filter2([N | Rest], V) when N >= $0, N =< $9 ->
    filter2(Rest, [N | V]);
filter2([_ | Rest] = L, V) ->
    Numbers =
        lists:enumerate(
            $1,
            ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        ),
    Match = [D || {D, Text} <- Numbers, lists:prefix(Text, L)],
    filter2(Rest, Match ++ V).
