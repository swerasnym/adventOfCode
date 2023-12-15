-module(advent).
-export([run/0]).

run() ->
    application:load(aoc),
    % find_all_aoc_solutions().
    [aoc_solution:run(M) || M <- find_all_aoc_solutions()].

find_all_aoc_solutions() ->
    {ok, aoc} = application:get_application(?MODULE),
    {ok, Modules} = application:get_key(aoc, modules),
    ok = code:atomic_load(Modules),
    lists:filter(fun is_aoc_solution/1, Modules).

is_aoc_solution(Module) ->
    Attributes = erlang:get_module_info(Module, attributes),
    % io:format("~p, ~p", [Module, Attributes]),
    case proplists:get_value(behaviour, Attributes) of
        undefined ->
            false;
        List ->
            lists:member(aoc_solution, List)
    end.
