-module(aoc2019_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Result = phases(100, Data),
    {First, _} = lists:split(8, Result),

    list_to_integer([C + $0 || C <- First]).

star2(Data) ->
    {First, _} = lists:split(7, Data),
    Offset = list_to_integer([C + $0 || C <- First]),

    {_, List} = split(Offset, lists:flatten(lists:duplicate(10000, Data))),
    Result = phases2(100, List),

    {First2, _} = lists:split(8, Result),

    list_to_integer([C + $0 || C <- First2]).

read(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(string:trim(Bin)),
    [C - $0 || C <- String].

phases(0, List) ->
    List;
phases(N, List) ->
    phases(N - 1, phase2(List)).

phases2(0, List) ->
    List;
phases2(N, List) ->
    phases2(N - 1, phase3(List)).

phase2(List) ->
    [phase2(List, Position) || Position <- lists:seq(1, length(List))].

phase2(List, Position) ->
    % drop first
    {_First1, Rest1} = split(Position - 1, List),
    {First2, Rest2} = split(Position, Rest1),
    {_First3, Rest3} = split(Position, Rest2),
    {First4, Rest4} = split(Position, Rest3),
    phase2(Rest4, Position, lists:sum(First2) - lists:sum(First4)).

phase2([], _Position, Acc) ->
    abs(Acc) rem 10;
phase2(List, Position, Acc) ->
    {_First1, Rest1} = split(Position, List),
    {First2, Rest2} = split(Position, Rest1),
    {_First3, Rest3} = split(Position, Rest2),
    {First4, Rest4} = split(Position, Rest3),
    phase2(Rest4, Position, Acc + lists:sum(First2) - lists:sum(First4)).

split(N, List) ->
    case N > length(List) of
        true ->
            {List, []};
        false ->
            lists:split(N, List)
    end.

phase3(List) ->
    Reversed = lists:reverse(List),
    calculate(Reversed, 0, []).

calculate([], _Prev, Acc) ->
    Acc;
calculate([N | Ns], Prev, Acc) ->
    Value = abs(N + Prev) rem 10,
    calculate(Ns, Value, [Value | Acc]).
