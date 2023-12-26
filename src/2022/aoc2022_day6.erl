-module(aoc2022_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 6}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    [Data] = tools:read_lines(File),
    Data.

star1(Data) ->
    start(4, Data).

star2(Data) ->
    start_m(14, Data).

start(Pos, [A | Rest = [B, C, D | _]]) ->
    case length(lists:uniq([A, B, C, D])) of
        4 ->
            Pos;
        _ ->
            start(Pos + 1, Rest)
    end.

start_m(Pos, [_ | Rest] = L) ->
    case length(lists:uniq(lists:sublist(L, 14))) of
        14 ->
            Pos;
        _ ->
            start_m(Pos + 1, Rest)
    end.
