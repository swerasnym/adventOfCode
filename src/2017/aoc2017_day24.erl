-module(aoc2017_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day24_ex.txt", star1, 31},
        {"examples/2017/day24_ex.txt", star2, 19}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Components) ->
    dfs1(0, Components, 0).

star2(Components) ->
    {_, S} = dfs2(0, Components, 0, 0),
    S.

read(File) ->
    tools:group(2, tools:read_integers(File, "/\n")).

connect(P, {P, O}) -> O;
connect(P, {O, P}) -> O.

strength({P1, P2}) -> P1 + P2.

dfs1(_, [], S) ->
    S;
dfs1(Port, Components, S) ->
    Fits = [C || C = {P1, P2} <- Components, P1 == Port orelse P2 == Port],
    Under = [dfs1(connect(Port, C), Components -- [C], S + strength(C)) || C <- Fits],
    tools:max_or(Under, S).
dfs2(_, [], D, S) ->
    {D, S};
dfs2(Port, Components, D, S) ->
    Fits = [C || C = {P1, P2} <- Components, P1 == Port orelse P2 == Port],
    Under = [dfs2(connect(Port, C), Components -- [C], D + 1, S + strength(C)) || C <- Fits],
    tools:max_or(Under, {D, S}).
