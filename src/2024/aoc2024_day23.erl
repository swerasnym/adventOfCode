-module(aoc2024_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day23_ex.txt", star1, 7},
        {"examples/2024/day23_ex.txt", star2, "co,de,ka,ta"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(G) ->
    length(
        lists:usort(lists:flatten([tipples(A, AN, B, G) || [$t, _] = A := AN <- G, B <- AN]))
    ).

star2(G) ->
    CS = find_cliques(G),
    {_, L} = lists:max(CS),
    io:format("~p~n", [lists:sort(CS)]),
    string:join(L, ",").

read(File) ->
    build_graph(tools:read_lines(File, fun(L) -> string:split(L, "-") end), #{}).

build_graph([], G) ->
    #{A => lists:sort(AN) || A := AN <- G};
build_graph([[A, B] | Rest], G) ->
    AN = maps:get(A, G, []),
    BN = maps:get(B, G, []),
    build_graph(Rest, G#{A => [B | AN], B => [A | BN]}).

tipples(A, AN, B, G) ->
    BN = maps:get(B, G),
    CN = tools:overlap(AN, BN),
    [list_to_tuple(lists:sort([A, B, C])) || C <- CN].

find_cliques(G) ->
    Nodes = maps:keys(G),
    find_cliques(G, [], lists:sort(Nodes), [], []).

find_cliques(_G, R, [], X, Result) ->
    case length(X) of
        0 ->
            [{length(R), R} | Result];
        _ ->
            Result
    end;
find_cliques(G, R, [V | Pr] = P, X, Result) ->
    N = maps:get(V, G),
    Res = find_cliques(
        G, R ++ [V], tools:overlap(P, N), tools:overlap(X, N), Result
    ),
    find_cliques(G, R, Pr, X ++ [V], Res).
