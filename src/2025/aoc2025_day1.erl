-module(aoc2025_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day1_ex.txt", star1, 3},
        {"examples/2025/day1_ex.txt", star2, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    count_zero(50, Data, 0).

star2(Data) ->
    turn(50, Data, 0).

read(File) ->
    tools:read_lines(File, fun parse_dir/1).

parse_dir("L" ++ Number) ->
    -list_to_integer(Number);
parse_dir("R" ++ Number) ->
    list_to_integer(Number).

count_zero(0, [], Count) ->
    Count + 1;
count_zero(_Pos, [], Count) ->
    Count;
count_zero(0, [P | Ps], Count) ->
    count_zero(tools:mod(P, 100), Ps, Count + 1);
count_zero(Pos, [P | Ps], Count) ->
    count_zero(tools:mod(Pos + P, 100), Ps, Count).

turn(0, [], Count) ->
    Count + 1;
turn(_Pos, [], Count) ->
    Count;
turn(0, [0 | Rest], Count) ->
    io:format("Pos 0, Count ~p~n", [Count]),
    turn(0, Rest, Count);
turn(Pos, [0 | Rest], Count) ->
    io:format("Pos ~p, Count ~p~n", [Pos, Count]),
    turn(Pos, Rest, Count);
turn(0, [P | Rest], Count) ->
    S = tools:sign(P),
    turn(S, [P - S | Rest], Count + 1);
turn(Pos, [P | Rest], Count) ->
    S = tools:sign(P),
    turn(tools:mod(Pos + S, 100), [P - S | Rest], Count).

