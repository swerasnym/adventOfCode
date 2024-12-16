-module(aoc_graph_tests).
-include_lib("eunit/include/eunit.hrl").

no_path_test() ->
    ?assertEqual(
        {no_path, #{1 => {0, [start]}, 2 => {0, [start]}}},
        aoc_graph:dijkstra(
            [1, 2],
            fun(_) -> false end,
            fun(_) -> [] end
        ),
        "The empty state of gb_sets have probably been updated"
    ).
