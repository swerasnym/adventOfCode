-module(aoc2015_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2015/day17_ex.txt", {star1, 25}, 4},
        {"examples/2015/day17_ex.txt", {star2, 25}, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(EmptyJugs) ->
    star1(EmptyJugs, 150).

star1(EmptyJugs, Volume) ->
    {W, _} = ways1(EmptyJugs, Volume, #{}),
    W.

star2(EmptyJugs) ->
    star2(EmptyJugs, 150).

star2(EmptyJugs, Volume) ->
    {{W, J}, _} = ways2(EmptyJugs, Volume, #{}),
    io:format("Min no of jugs: ~p in ~p ways~n", [J, W]),
    W.

read(File) ->
    tools:read_integers(File).

ways1(_, 0, M) ->
    {1, M};
ways1(_, Left, M) when Left < 0 ->
    {0, M};
ways1([], Left, M) when Left > 0 ->
    {0, M};
ways1([J | Rest] = EmptyJugs, Left, Memory) ->
    Key = {length(EmptyJugs), Left},
    case maps:get(Key, Memory, unknown) of
        unknown ->
            {W1, M1} = ways1(Rest, Left - J, Memory),
            {W2, M2} = ways1(Rest, Left, M1),
            W = W1 + W2,
            {W, M2#{Key => W}};
        Value ->
            {Value, Memory}
    end.

ways2(_, 0, M) ->
    {{1, 0}, M};
ways2(_, Left, M) when Left < 0 ->
    {{0, 0}, M};
ways2([], Left, M) when Left > 0 ->
    {{0, 0}, M};
ways2([J | Rest] = EmptyJugs, Left, Memory) ->
    Key = {length(EmptyJugs), Left},
    case maps:get(Key, Memory, unknown) of
        unknown ->
            {{W1, J1}, M1} = ways2(Rest, Left - J, Memory),
            {{W2, J2}, M2} = ways2(Rest, Left, M1),

            Result =
                case {W1, W2} of
                    {0, _} ->
                        {W2, J2};
                    {_, 0} ->
                        {W1, J1 + 1};
                    _ when J1 + 1 == J2 ->
                        {W1 + W2, J2};
                    _ when J1 + 1 < J2 ->
                        {W1, J1 + 1};
                    _ when J1 + 1 > J2 ->
                        {W2, J2}
                end,
            {Result, M2#{Key => Result}};
        Value ->
            {Value, Memory}
    end.
