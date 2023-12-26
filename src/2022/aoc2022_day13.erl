-module(aoc2022_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 13}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_blocks(File, parse_lines).

star1(Data) ->
    Comparasons = [compare(D) || D <- Data],
    lists:sum([I || {I, true} <- lists:enumerate(Comparasons)]).

star2(Data) ->
    Packets = ["[[2]]", "[[6]]"] ++ lists:merge(Data),
    Sorted = lists:sort(fun(A, B) -> compare([A, B]) end, Packets),
    % [io:format("~s~n", [L]) || L <- Sorted],
    tools:product([I || {I, V} <- lists:enumerate(Sorted), V == "[[2]]" orelse V == "[[6]]"]).

compare([A, B]) ->
    compare(tools:as_term(A), tools:as_term(B)).

compare(A, A) ->
    equal;
compare([A | RestA], [A | RestB]) ->
    compare(RestA, RestB);
compare([A | _RestA], [B | _RestB]) when is_integer(A), is_integer(B) ->
    A < B;
compare([A | RestA], [B | RestB]) when is_integer(A), is_list(B) ->
    compare([[A] | RestA], [B | RestB]);
compare([A | RestA], [B | RestB]) when is_integer(B), is_list(A) ->
    compare([A | RestA], [[B] | RestB]);
compare([], [_B | _]) ->
    true;
compare([_A | _], []) ->
    false;
compare([A | RestA], [B | RestB]) when is_list(A), is_list(B) ->
    case compare(A, B) of
        equal ->
            compare(RestA, RestB);
        Res ->
            Res
    end.
