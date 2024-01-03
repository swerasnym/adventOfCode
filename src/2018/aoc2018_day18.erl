-module(aoc2018_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day18_ex.txt", star1, 1147}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    tools:print_grid(Map),
    End = tools:repeat(10, fun step/2, Map),
    #{$# := Lumberyards, $| := Trees} = tools:count(End),
    Lumberyards * Trees.

star2(Map) ->
    erlang:erase(),
    End = simulate(Map, 1000000000),
    tools:print_grid(End),
    #{$# := Lumberyards, $| := Trees} = tools:count(End),
    Lumberyards * Trees.

read(File) ->
    tools:drop_max(tools:read_grid(File)).

step(Map) ->
    maps:map(fun(K, V) -> update(K, V, Map) end, Map).

step(N, Map) ->
    Out = step(Map),
    io:format("~n~p~n", [N]),
    tools:print_grid(Out),
    Out.

neighbours({X, Y}) ->
    [{X + Dx, Y + Dy} || Dx <- [-1, 0, 1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}].

update(Pos, $., Map) ->
    case tools:count($|, [maps:get(P, Map, outside) || P <- neighbours(Pos)]) of
        N when N >= 3 -> $|;
        _ -> $.
    end;
update(Pos, $|, Map) ->
    case tools:count($#, [maps:get(P, Map, outside) || P <- neighbours(Pos)]) of
        N when N >= 3 -> $#;
        _ -> $|
    end;
update(Pos, $#, Map) ->
    case tools:count([maps:get(P, Map, outside) || P <- neighbours(Pos)]) of
        #{$# := _, $| := _} -> $#;
        _ -> $.
    end.

simulate(Map, 0) ->
    Map;
simulate(Map, N) ->
    case erlang:get(Map) of
        undefined ->
            erlang:put(Map, N),
            simulate(step(Map), N - 1);
        Pn ->
            erlang:erase(),
            Dn = Pn - N,
            Cycles = N div Dn,
            simulate(Map, N - Dn * Cycles)
    end.
