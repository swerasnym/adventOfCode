-module(aoc2022_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 3}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
read(File) ->
    tools:read_lines(File).

star1(Data) ->
    lists:sum([overlap(D) || D <- Data]).

star2(Data) ->
    badge(Data, 0).

overlap(L) ->
    {L1, L2} = lists:split(length(L) div 2, L),
    S1 = sets:from_list(L1),
    S2 = sets:from_list(L2),
    S = sets:intersection(S1, S2),
    [I] = sets:to_list(S),
    prio(I).

prio(L) when L >= $a, L =< $z ->
    L - $a + 1;
prio(L) when L >= $A, L =< $Z ->
    L - $A + 27.

badge([], Sum) ->
    Sum;
badge([L1, L2, L3 | Rest], Sum) ->
    S1 = sets:from_list(L1),
    S2 = sets:from_list(L2),
    S3 = sets:from_list(L3),
    S = sets:intersection([S1, S2, S3]),
    [I] = sets:to_list(S),
    badge(Rest, Sum + prio(I)).
