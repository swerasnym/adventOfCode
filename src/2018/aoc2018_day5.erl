-module(aoc2018_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "aA"}, star1, 0},
        {{data, "abBA"}, star1, 0},
        {{data, "abAB"}, star1, 4},
        {{data, "aabAAB"}, star1, 6},
        {"examples/2018/day5_ex.txt", star1, 10},
        {"examples/2018/day5_ex.txt", star2, 4}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Polymer) ->
    react(Polymer).

star2(Polymer) ->
    Reactions = [
        react(lists:filter(fun(V) -> not lists:member(V, [C, C - $A + $a]) end, Polymer))
     || C <- lists:seq($A, $Z)
    ],
    lists:min(Reactions).

read(File) ->
    tools:read_string(File).

react(Polymer) ->
    react(Polymer, []).

react([], Reacted) ->
    length(Reacted);
react([H1 | Rest], []) ->
    react(Rest, [H1]);
react([H1 | Rest1], [H2 | Rest2] = All2) ->
    case same(H1, H2) of
        true ->
            react(Rest1, Rest2);
        false ->
            react(Rest1, [H1 | All2])
    end.

same(A, B) when A > B ->
    (A - $a) == (B - $A);
same(A, B) when A < B ->
    (A - $A) == (B - $a);
same(_, _) ->
    false.
