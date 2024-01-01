-module(aoc_watch_code_tests).
-include_lib("eunit/include/eunit.hrl").

set_get_test_() ->
    [set_get(lists:seq(1, N)) || N <- lists:seq(1, 10)].

set_get(List) ->
    State0 = aoc_watch_code:new(length(List)),
    State1 = aoc_watch_code:set_registers(State0, List),
    ?_assertEqual(List, aoc_watch_code:get_registers(State1)).
