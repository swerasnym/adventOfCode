-module(aoc2017_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2017/day4_ex.txt", star1, unknown},
        %{"examples/2017/day4_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Phrases) ->
    length([P || P <- Phrases, valid(P)]).

star2(Phrases) ->
    length([P || P <- Phrases, valid(sort_words(P))]).

read(File) ->
    tools:read_lines(File, {fun string:split/3, [" ", all]}).

valid(PassPhrase) ->
    length(PassPhrase) == length(lists:usort(PassPhrase)).

sort_words(PassPhrase) ->
    [lists:sort(P) || P <- PassPhrase].
