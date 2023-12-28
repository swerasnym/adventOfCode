-module(aoc2022_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 4}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

%% Note started 15 min late
read(File) ->
    tools:read_lines(
        File,
        fun(L) ->
            [A, B, C, D] = tools:parse_format(L, "~d-~d,~d-~d"),
            {{A, B}, {C, D}}
        end
    ).

star1(Data) ->
    length(lists:filter(fun contained/1, Data)).

star2(Data) ->
    length(lists:filter(fun overlaps/1, Data)).

contained({{A, B}, {C, D}}) when A >= C, B =< D ->
    true;
contained({{A, B}, {C, D}}) when C >= A, D =< B ->
    true;
contained(_) ->
    false.

overlaps({{_, B}, {C, D}}) when C =< B, B =< D ->
    true;
overlaps({{A, _}, {C, D}}) when C =< A, A =< D ->
    true;
overlaps({{A, B}, {_, D}}) when A =< D, D =< B ->
    true;
overlaps({{A, B}, {C, _}}) when A =< C, C =< B ->
    true;
overlaps(_) ->
    false.
