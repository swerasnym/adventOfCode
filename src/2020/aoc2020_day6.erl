-module(aoc2020_day6).
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
        problem => {2020, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Data) ->
    Anyone = [length(lists:usort(lists:flatten(Group))) || Group <- Data],
    lists:sum(Anyone).

star2(Data) ->
    Evryone =
        [
            begin
                Count = tools:count(lists:flatten(Group)),
                [K || {K, V} <- maps:to_list(Count), V == length(Group)]
            end
         || Group <- Data
        ],
    lists:flatlength(Evryone).

read(File) ->
    [tools:parse_lines(Group) || Group <- tools:read_blocks(File)].
