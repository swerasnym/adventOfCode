-module(aoc2023_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day9_ex1.txt", star1, 114},
        {"examples/2023/day9_ex1.txt", star2, 2}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:sum([extrrapolate(D, fwd) || D <- Data]).

star2(Data) ->
    lists:sum([extrrapolate(D, rev) || D <- Data]).

read(File) ->
    tools:read_lines(File, parse_integers).

extrrapolate(List, Dir) ->
    case all_zero(List) of
        true ->
            0;
        false when Dir == fwd ->
            lists:last(List) + extrrapolate(dderiv(List, []), Dir);
        false when Dir == rev ->
            hd(List) - extrrapolate(dderiv(List, []), Dir)
    end.
dderiv([_], Res) ->
    lists:reverse(Res);
dderiv([A | [B | _] = Rest], Res) ->
    dderiv(Rest, [B - A | Res]).

all_zero([]) ->
    true;
all_zero([0 | Rest]) ->
    all_zero(Rest);
all_zero(_) ->
    false.
