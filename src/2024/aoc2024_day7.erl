-module(aoc2024_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day7_ex.txt", star1, 3749},
        {"examples/2024/day7_ex.txt", star2, 11387}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:sum([R || {R, L} <- Data, solve(R, L)]).

star2(Data) ->
    lists:sum([R || {R, L} <- Data, solve2(R, L)]).

read(File) ->
    Lines = tools:read_lines(File, fun(L) -> tools:parse_integers(L, " :") end),
    [{Res, Values} || [Res | Values] <- Lines].

solve(Result, [A]) ->
    Result == A;
solve(Result, [A, B | Rest]) ->
    solve(Result, [A + B | Rest]) orelse solve(Result, [A * B | Rest]).

solve2(Result, [A]) ->
    Result == A;
solve2(Result, [A, B | Rest]) ->
    solve2(Result, [A + B | Rest]) orelse
        solve2(Result, [A * B | Rest]) orelse
        solve2(Result, [combine(A, B) | Rest]).

combine(A, B) ->
    list_to_integer(integer_to_list(A) ++ integer_to_list(B)).
