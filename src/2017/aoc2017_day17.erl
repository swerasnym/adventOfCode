-module(aoc2017_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {{data, 3}, star1, 638},
        {{data, 3}, {star2, 1}, 1},
        {{data, 3}, {star2, 2}, 2},
        {{data, 3}, {star2, 3}, 2},
        {{data, 3}, {star2, 4}, 2},
        {{data, 3}, {star2, 5}, 5},
        {{data, 3}, {star2, 6}, 5},
        {{data, 3}, {star2, 7}, 5},
        {{data, 3}, {star2, 8}, 5},
        {{data, 3}, {star2, 9}, 9}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(StepSize) ->
    [2017, Ans | _] = lists:foldl(insert(StepSize), [0], lists:seq(1, 2017)),
    Ans.
star2(StepSize) ->
    star2(StepSize, 50000000).

star2(StepSize, Times) ->
    {_, Res, _} = tools:repeat(Times, simulate(StepSize), {0, 0, 1}),
    Res.

read(File) ->
    list_to_integer(tools:read_string(File)).

insert(StepSize) ->
    fun(Val, List) ->
        [Val | tools:rotate(StepSize + 1, List)]
    end.

simulate(StepSize) ->
    fun({ZeroPos, After, Size}) ->
        NewZero = tools:mod(ZeroPos - StepSize, Size),
        case NewZero == Size - 1 of
            true -> {NewZero, Size, Size + 1};
            false -> {NewZero, After, Size + 1}
        end
    end.
