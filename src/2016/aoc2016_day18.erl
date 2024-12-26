-module(aoc2016_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "..^^."}, {star1, 3}, 6},
        {{data, ".^^.^.^^^^"}, {star1, 10}, 38}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Row1) ->
    star1(Row1, 40).

star1(Row1, Rows) ->
    Map0 = #{{X, 1} => S || {X, S} <- lists:enumerate(Row1)},
    New = [{X, Y} || Y <- lists:seq(2, Rows), X <- lists:seq(1, length(Row1))],
    Map = lists:foldl(fun add_tile/2, Map0, New),
    tools:print_grid(Map),
    tools:count($., Map).

star2(Row1) ->
    Binary1 = list_to_binary(Row1),
    solve2(400000, Binary1, tools:count($., Binary1)).

read(File) ->
    tools:read_string(File).

add_tile({X, Y} = Pos, Map) ->
    Prev = [maps:get({XP, Y - 1}, Map, $.) || XP <- lists:seq(X - 1, X + 1)],
    Map#{Pos => type(Prev)}.

type(Binary, Idx) ->
    type([at(I, Binary) || I <- lists:seq(Idx - 1, Idx + 1)]).

type("^^.") -> $^;
type(".^^") -> $^;
type("..^") -> $^;
type("^..") -> $^;
type(_) -> $..

solve2(1, _, Count) ->
    Count;
solve2(N, Binary, Count) ->
    Next = <<<<(type(Binary, I))>> || I <- lists:seq(0, byte_size(Binary) - 1)>>,
    solve2(N - 1, Next, Count + tools:count($., Next)).

at(-1, _) -> $.;
at(N, Binary) when N == byte_size(Binary) -> $.;
at(N, Binary) -> binary:at(Binary, N).
