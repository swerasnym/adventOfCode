-module(aoc2016_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 25}
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({BunA0, BunB0}) ->
    BunA = aoc_bun:set_opt(jint, true, BunA0),
    EndA = aoc_bun:run(BunA),
    #{a := Add} = Mem = aoc_bun:get_mem(EndA),
    A = find_a(Add, 2),
    BunB1 = aoc_bun:set_opt(jint, true, BunB0),
    Start = Mem#{a => A + Add},
    BunB = aoc_bun:set_mem(BunB1, Start),
    EndB = aoc_bun:run(BunB),
    Output = aoc_bun:get_out(EndB),
    true = verify_output(0, Output),
    io:format("~p~n", [Output]),
    A.

star2(_) ->
    {done, "Transmit the Signal!"}.

read(File) ->
    {P1, P2} = lists:split(9, tools:read_lines(File)),
    {aoc_bun:from_lines(P1), aoc_bun:from_lines(P2)}.

find_a(Add, N) when N > Add ->
    N - Add;
find_a(Add, N) ->
    find_a(Add, N * 4 + 2).

verify_output(0, []) ->
    true;
verify_output(0, [0 | Rest]) ->
    verify_output(1, Rest);
verify_output(1, [1 | Rest]) ->
    verify_output(0, Rest);
verify_output(_, _) ->
    false.
