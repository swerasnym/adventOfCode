-module(aoc2020_day15).

-export([run/2]).

run(Star, File) ->
    Data = read(File),

    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

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
