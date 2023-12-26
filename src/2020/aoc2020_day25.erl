-module(aoc2020_day25).
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
        problem => {2020, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1([P1, P2]) ->
    transform2(transform1(0, 1, P1), P2, 1).

star2(_Data) ->
    {done, "Pay Deposit!"}.

read(File) ->
    tools:read_integers(File).

transform2(0, _S, R) ->
    R;
transform2(N, S, V) ->
    transform2(N - 1, S, V * S rem 20201227).

transform1(N, E, E) ->
    N;
transform1(N, V, E) ->
    transform1(N + 1, V * 7 rem 20201227, E).
