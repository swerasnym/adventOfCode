-module(day10).

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

star1(Sorted) ->
    #{1 := One, 3 := Three} = tools:count(diffrences(Sorted)),
    One * Three.

star2(Sorted) ->
    Diff = diffrences(Sorted),
    tools:product([multiplicity(Rep) || Rep <- count_repeeted_ones(Diff)]).

read(File) ->
    Sorted = tools:read_integers(File, sort),
    [0] ++ Sorted ++ [lists:last(Sorted) + 3].

diffrences(Sorted) ->
    diffrences(Sorted, []).

diffrences([_], Diffrences) ->
    Diffrences;
diffrences([A, B | Sorted], Diffrences) ->
    diffrences([B | Sorted], [B - A | Diffrences]).

count_repeeted_ones(Diff) ->
    count_repeeted_ones(Diff, 0, []).

count_repeeted_ones([], _Count, Res) ->
    Res;
count_repeeted_ones([1 | Diff], Count, Res) ->
    count_repeeted_ones(Diff, Count + 1, Res);
count_repeeted_ones([3 | Diff], Count, Res) ->
    count_repeeted_ones(Diff, 0, [Count | Res]).

multiplicity(0) ->
    1;
multiplicity(1) ->
    1;
multiplicity(2) ->
    2;
multiplicity(3) ->
    4;
multiplicity(4) ->
    7.
