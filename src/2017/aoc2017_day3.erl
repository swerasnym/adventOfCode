-module(aoc2017_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, 1}, star1, 0},
        {{data, 12}, star1, 3},
        {{data, 9}, star1, 2},
        {{data, 25}, star1, 4},
        {{data, 23}, star1, 2},
        {{data, 1024}, star1, 31},
        {{data, 23}, star2, 25}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Input) ->
    dist(pos(Input)).

star2(Input) ->
    search(2, #{{0, 0} => 1}, Input).

read(File) ->
    [Input] = tools:read_integers(File),
    Input.

odd_sqrt(N) ->
    S = erlang:trunc(math:sqrt(N)),
    case S rem 2 of
        0 -> S - 1;
        1 -> S
    end.

pos(Num) ->
    Sr = odd_sqrt(Num),
    K = Sr div 2,
    Left = Num - Sr * Sr,
    Kp1 = K + 1,

    case {Left div (Sr + 1), Left rem (Sr + 1)} of
        _ when Left == 0 -> {K, K};
        {0, N} -> {Kp1, Kp1 - N};
        {1, N} -> {Kp1 - N, -Kp1};
        {2, N} -> {-Kp1, -Kp1 + N};
        {3, N} -> {-Kp1 + N, Kp1}
    end.

dist({X, Y}) ->
    abs(X) + abs(Y).

neighbours({X, Y}) ->
    [{X + Dx, Y + Dy} || Dx <- [-1, 0, 1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}].

search(Num, Map, Goal) ->
    Pos = pos(Num),
    Sum = lists:sum([maps:get(N, Map, 0) || N <- neighbours(Pos)]),
    case Sum > Goal of
        true -> Sum;
        false -> search(Num + 1, Map#{Pos => Sum}, Goal)
    end.
