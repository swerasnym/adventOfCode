-module(aoc_solution_tests).
-include_lib("eunit/include/eunit.hrl").

problem_set_test() ->
    Solutions = aoc_solution:get_all_solutions(),
    Missing = [M || M <- Solutions, maps:get(problem, M:info(), missing) == missing],
    ?assertEqual([], Missing, {"Problem not set", Missing}).

examples_test_() ->
    {inparallel, [
        {timeout, 300, verify_examples(M)}
     || M <- aoc_solution:get_all_released()
    ]}.
input_test_() ->
    {inparallel, [
        {timeout, 300, verify_input(M)}
     || M <- aoc_solution:get_all_released()
    ]}.

verify_examples(M) ->
    {
        "examples " ++ atom_to_list(M),
        {spawn, fun() ->
            Results = aoc_solution:run(M, all, examples),
            Failed = [{M, S} || {_, #{check := fail, star := S}} <- Results],
            ?assertEqual([], Failed, {"Problem failing for", Failed})
        end}
    }.

verify_input(M) ->
    {
        "input " ++ atom_to_list(M),
        {spawn, fun() ->
            Results = aoc_solution:run(M, both, input),
            Failed = [{M, S} || {_, #{check := fail, star := S}} <- Results],
            Filtered = lists:filter(fun({Mod, _}) -> maps:get(stable, Mod:info()) end, Failed),
            case {Failed, Filtered} of
                {[], _} ->
                    ok;
                {_, []} ->
                    ?debugFmt("Unstable: ~p", [Failed]);
                {_, _} ->
                    ?debugFmt("Falied: ~p", [Failed])
            end,

            ?assertEqual([], Filtered, {"Problem failing for", Filtered})
        end}
    }.
