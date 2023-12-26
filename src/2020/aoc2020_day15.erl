-module(aoc2020_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Turns = 2020,
    Start = length(Data) + 1,
    Ref = atomics:new(Turns + 1, []),
    [
        atomics:put(Ref, Value + 1, Turn)
     || {Value, Turn} <- lists:zip(Data, lists:seq(1, length(Data)))
    ],
    speek(Turns, 0, Start, Ref).

star2(Data) ->
    Turns = 30000000,
    Start = length(Data) + 1,
    Ref = atomics:new(Turns + 1, []),
    [
        atomics:put(Ref, Value + 1, Turn)
     || {Value, Turn} <- lists:zip(Data, lists:seq(1, length(Data)))
    ],
    speek(Turns, 0, Start, Ref).

read(File) ->
    tools:read_integers(File, ",").

speek(Goal, Last, Goal, _Ref) ->
    Last;
speek(Goal, Last, Turn, Ref) ->
    Speek =
        case atomics:exchange(Ref, Last + 1, Turn) of
            0 ->
                0;
            N ->
                Turn - N
        end,
    speek(Goal, Speek, Turn + 1, Ref).
