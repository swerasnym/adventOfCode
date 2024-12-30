-module(aoc2017_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day18_ex.txt", star1, 4},
        {"examples/2017/day18_ex2.txt", star2, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    {rcv, S} = aoc_tablet:run(Program),
    lists:last(aoc_tablet:get_output(S)).

star2(Program) ->
    P0 = aoc_tablet:set_mem(Program, #{p => 0}),
    P1 = aoc_tablet:set_mem(Program, #{p => 1}),
    loop(aoc_tablet:run(P0), aoc_tablet:run(P1), 0).

read(File) ->
    aoc_tablet:from_file(File).

loop({rcv, I0}, {rcv, I1}, Count) ->
    O0 = aoc_tablet:get_output(I0),
    O1 = aoc_tablet:get_output(I1),
    case {O0, O1} of
        {[], []} ->
            Count;
        _ ->
            R0 = aoc_tablet:add_input(I0, O1),
            R1 = aoc_tablet:add_input(I1, O0),
            P0 = aoc_tablet:clear_output(R0),
            P1 = aoc_tablet:clear_output(R1),

            loop(aoc_tablet:run(P0), aoc_tablet:run(P1), Count + length(O1))
    end.
