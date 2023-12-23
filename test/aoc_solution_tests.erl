-module(aoc_solution_tests).
-include_lib("eunit/include/eunit.hrl").

problem_set_test() ->
    Solutions = aoc_solution:get_all_solutions(),
    Missing = [M || M <- Solutions, maps:get(problem, M:info(), missing) == missing],
    ?assertEqual([], Missing, {"Problem not set", Missing}).

verify_results_test() ->
    Results = aoc_solution:run(aoc_solution:get_all_solutions(), all, examples),
    Failed = [{M, S} || {_, #{check := fail, module := M, star := S}} <- Results],
    ?assertEqual([], Failed, {"Problem failing for", Failed}).
