-module(aoc2021_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    larger(Data, 0).

star2(Data) ->
    larger(window(Data, []), 0).

read(File) ->
    tools:read_integers(File).

larger([], Result) ->
    Result;
larger([A, B | Rest], Result) when A < B ->
    larger([B | Rest], Result + 1);
larger([_ | Rest], Result) ->
    larger(Rest, Result).

window([A | [B, C | _] = Rest], Result) ->
    window(Rest, [A + B + C | Result]);
window(_, Result) ->
    lists:reverse(Result).
