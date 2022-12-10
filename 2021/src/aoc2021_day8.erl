-module(aoc2021_day8).

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
    lists:sum([count_low(OP) || {_, OP} <- Data]).

star2(Data) ->
    lists:sum([num(D) || D <- Data]).

read(File) ->
    Lines = tools:read_lines(File),
    Processed = [string:tokens(Line, " ") || Line <- Lines],
    [{lists:sublist(L, 10), lists:sublist(L, 12, 4)} || L <- Processed].

num({Values, Number}) ->
    VM = maps:from_list(find_values(Values)),
    [A, B, C, D] = [maps:get(lists:sort(C), VM) || C <- Number],
    1000 * A + 100 * B + 10 * C + D.

count_low(OP) ->
    length(lists:filter(fun low/1, OP)).

low(Digit) ->
    is_number(possible_values(Digit)).

find_values({List, _}) ->
    find_values(List);
find_values(List) ->
    Values = [{possible_values(V), V} || V <- List],
    V069 =
        lists:filter(fun ({Vs, _}) ->
                             Vs == [0, 6, 9];
                         (_) ->
                             false
                     end,
                     Values),

    V235 =
        lists:filter(fun ({Vs, _}) ->
                             Vs == [2, 3, 5];
                         (_) ->
                             false
                     end,
                     Values),

    {1, C1} = lists:keyfind(1, 1, Values),
    {4, C4} = lists:keyfind(4, 1, Values),
    {7, C7} = lists:keyfind(7, 1, Values),
    {8, C8} = lists:keyfind(8, 1, Values),

    {[{_, C3}], V25} =
        lists:partition(fun({_, C}) -> all_of(C, C1) end, V235), %% 3 only with all of 1
    {[{_, C9}], V06} =
        lists:partition(fun({_, C}) -> all_of(C, C4) end, V069), %% 9 only with all of 4
    {[{_, C0}], [{_, C6}]} =
        lists:partition(fun({_, C}) -> all_of(C, C7) end, V06), %% 0 only with all of 7
    {[{_, C5}], [{_, C2}]} =
        lists:partition(fun({_, C}) -> all_of(C6, C) end, V25), %% 6 only with all of 5
    [{lists:sort(C1), 1},
     {lists:sort(C2), 2},
     {lists:sort(C3), 3},
     {lists:sort(C4), 4},
     {lists:sort(C5), 5},
     {lists:sort(C6), 6},
     {lists:sort(C7), 7},
     {lists:sort(C8), 8},
     {lists:sort(C9), 9},
     {lists:sort(C0), 0}].

possible_values(L) when is_list(L) ->
    possible_values(length(L));
possible_values(2) ->
    1;
possible_values(3) ->
    7;
possible_values(4) ->
    4;
possible_values(5) ->
    [2, 3, 5];
possible_values(6) ->
    [0, 6, 9];
possible_values(7) ->
    8.

all_of(List, Cmp) ->
    lists:all(fun(V) -> V end, [lists:member(C, List) || C <- Cmp]).
