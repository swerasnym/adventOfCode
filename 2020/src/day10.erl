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

star1(Data) ->
    Sorted = lists:sort(Data),
    Device = lists:last(Sorted) + 3,
    count([0] ++ Sorted ++ [Device], 0, 0).

star2(Data) ->
    Sorted = lists:sort(Data),
    Device = lists:last(Sorted) + 3,
    Diff = diffrences([0] ++ Sorted ++ [Device]),
    Rep = count_repeeted_ones(Diff),
    io:format("~p~n~p~n", [Diff, Rep]),
    product(Rep).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [list_to_integer(Line)
     || Line
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].

count([A], One, Tree) ->
    One * Tree;
count([A, B | Sorted], One, Tree) when A + 1 == B ->
    io:format("~p -> ~p (1)~n", [A, B]),
    count([B | Sorted], One + 1, Tree);
count([A, B | Sorted], One, Tree) when A + 3 == B ->
    io:format("~p -> ~p (3)~n", [A, B]),
    count([B | Sorted], One, Tree + 1);
count([A | Sorted], One, Tree) ->
    count([Sorted], One, Tree).

diffrences(Sorted) ->
    diffrences(Sorted, []).

diffrences([A], Diffrences) ->
    lists:reverse(Diffrences);
diffrences([A, B | Sorted], Diffrences) ->
    diffrences([B | Sorted], [B - A | Diffrences]).

count_repeeted_ones(Diff) ->
    count_repeeted_ones(Diff, 0, []).

count_repeeted_ones([], Count, Res) ->
    lists:reverse(Res);
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

product(List) ->
    product(List, 1).

product([], N) ->
    N;
product([A | List], N) ->
    product(List, N * multiplicity(A)).
