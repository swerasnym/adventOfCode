-module(aoc_solution_tests).
-include_lib("eunit/include/eunit.hrl").

problem_set_test() ->
    Solutions = aoc_solution:get_all_solutions(),
    Missing = [M || M <- Solutions, maps:get(problem, M:info(), missing) == missing],
    ?assertEqual([], Missing, "Some modules with aoc_solution behaviour are not seting problem!").
