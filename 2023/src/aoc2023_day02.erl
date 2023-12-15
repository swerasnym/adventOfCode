-module(aoc2023_day02).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 2}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:sum([Game || {Game, Colors} <- Data, possible(Colors)]).

star2(Data) ->
    lists:sum([power(Colors) || {_Game, Colors} <- Data]).

read(File) ->
    tools:read_lines(File, fun parse_game/1).

parse_game("Game " ++ Line) ->
    {ok, [Game], Rest} = io_lib:fread("~d: ", Line),

    {Game, [parse_color(V) || V <- tools:parse_format(Rest, "~d ~s")]}.

parse_color([N, "red" ++ _]) ->
    {red, N};
parse_color([N, "green" ++ _]) ->
    {green, N};
parse_color([N, "blue" ++ _]) ->
    {blue, N}.

possible([]) ->
    true;
possible([{red, N} | Rest]) when N < 13 ->
    possible(Rest);
possible([{green, N} | Rest]) when N < 14 ->
    possible(Rest);
possible([{blue, N} | Rest]) when N < 15 ->
    possible(Rest);
possible(_) ->
    false.

power(Colors) ->
    power(Colors, 0, 0, 0).

power([], R, G, B) ->
    R * G * B;
power([{red, N} | Rest], R, G, B) ->
    power(Rest, max(R, N), G, B);
power([{green, N} | Rest], R, G, B) ->
    power(Rest, R, max(G, N), B);
power([{blue, N} | Rest], R, G, B) ->
    power(Rest, R, G, max(B, N)).
