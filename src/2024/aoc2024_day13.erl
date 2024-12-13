-module(aoc2024_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day13_ex.txt", star1, 480},
        {"examples/2024/day13_ex.txt", star2, 875318608908}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 13},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:sum([solve(A, B, C) || {A, B, C} <- Data]).

star2(Data) ->
    lists:sum([solve(A, B, aoc_vector:add(10000000000000, C)) || {A, B, C} <- Data]).

read(File) ->
    tools:read_blocks(File, fun parse_block/1).

parse_block(Block) ->
    [A, B, P] = tools:parse_lines(Block),
    {
        tools:parse_format(A, "Button A: X+~d, Y+~d"),
        tools:parse_format(B, "Button B: X+~d, Y+~d"),
        tools:parse_format(P, "Prize: X=~d, Y=~d")
    }.

solve(A = [Ax, Ay], B = [Bx, By], C = [Cx, Cy]) ->
    Det = (Ax * By - Bx * Ay),
    Ap = (Cx * By - Bx * Cy) div Det,
    Bp = (Ax * Cy - Cx * Ay) div Det,
    case aoc_vector:add(aoc_vector:mul(Ap, A), aoc_vector:mul(Bp, B)) == C of
        true ->
            3 * Ap + Bp;
        false ->
            0
    end.
