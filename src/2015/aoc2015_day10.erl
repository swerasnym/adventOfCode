-module(aoc2015_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Data = {data, "1"},

    Examples = [
        {Data, {star1, 1}, "11"},
        {Data, {star1, 2}, "21"},
        {Data, {star1, 3}, "1211"},
        {Data, {star1, 4}, "111221"},
        {Data, {star1, 5}, "312211"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    length(star1(Data, 40)).

star1(Start, N) ->
    tools:repeat(N, fun look_and_say/1, Start).

star2(Data) ->
    length(tools:repeat(50, fun look_and_say/1, Data)).

read(File) ->
    tools:read_string(File).

look_and_say(String) ->
    Runs = runs(String, 0, []),
    lists:flatten([[erlang:integer_to_list(V), K] || {K, V} <- Runs]).

runs([], 0, Runs) ->
    lists:reverse(Runs);
runs([A], Count, Runs) ->
    runs([], 0, [{A, Count + 1} | Runs]);
runs([A, A | Rest], Count, Runs) ->
    runs([A | Rest], Count + 1, Runs);
runs([A, B | Rest], Count, Runs) ->
    runs([B | Rest], 0, [{A, Count + 1} | Runs]).
