-module(aoc2020_day2).
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
        problem => {2020, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    length([Line || Line <- Data, check_pwd1(Line)]).

star2(Data) ->
    length([Line || Line <- Data, check_pwd2(Line)]).

read(File) ->
    tools:read_multiple_formats(File, "~d-~d ~c:~s").

check_pwd1([Min, Max, [Char], Password]) ->
    Len = length([P || P <- Password, P == Char]),
    Len >= Min andalso Len =< Max.

check_pwd2([Idx1, Idx2, [Char], Password]) ->
    Pos1 = lists:nth(Idx1, Password),
    Pos2 = lists:nth(Idx2, Password),
    (Pos1 == Char) xor (Pos2 == Char).
