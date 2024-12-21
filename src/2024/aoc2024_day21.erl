-module(aoc2024_day21).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day21_ex.txt", star1, 126384},
        {"examples/2024/day21_ex.txt", star2, 154115708116294}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Codes) ->
    R1Costs = calculate_cost(keypad_controller(), me_costs()),
    R2Costs = calculate_cost(keypad_controller(), R1Costs),
    DoorCosts = calculate_cost(keypad_door(), R2Costs),

    lists:sum([numeric(Code) * press($A, Code, DoorCosts, 0) || Code <- Codes]).

star2(Codes) ->
    R25Costs = nested_costs(25, me_costs()),
    DoorCosts = calculate_cost(keypad_door(), R25Costs),

    lists:sum([numeric(Code) * press($A, Code, DoorCosts, 0) || Code <- Codes]).

read(File) ->
    tools:read_lines(File).

nested_costs(0, Costs) ->
    Costs;
nested_costs(N, Costs) ->
    nested_costs(N - 1, calculate_cost(keypad_controller(), Costs)).

press(_, "", _, C) ->
    C;
press(F, [T | Rest], Map, C) ->
    press(T, Rest, Map, C + maps:get([F, T], Map)).

numeric(Code) ->
    list_to_integer(lists:flatten(string:replace(Code, "A", "", all))).

%% erlfmt:ignore Make the layout visible
keypad_door() ->
    #{
        {0, 0} => $7, {1, 0} => $8, {2, 0} => $9,
        {0, 1} => $4, {1, 1} => $5, {2, 1} => $6,
        {0, 2} => $1, {1, 2} => $2, {2, 2} => $3,
                      {1, 3} => $0, {2, 3} => $A
    }.

%% erlfmt:ignore Make the layout visible
keypad_controller() ->
    #{
                      {1, 0} => $^, {2, 0} => $A,
        {0, 1} => $<, {1, 1} => $v, {2, 1} => $>
    }.

move(Pos, $^) -> aoc_vector:add(Pos, {0, -1});
move(Pos, $v) -> aoc_vector:add(Pos, {0, 1});
move(Pos, $>) -> aoc_vector:add(Pos, {1, 0});
move(Pos, $<) -> aoc_vector:add(Pos, {-1, 0}).

calculate_cost(KeyPad, Costs) ->
    #{
        [K1, K2] => calculate_cost(P1, P2, KeyPad, Costs)
     || P1 := K1 <- KeyPad, P2 := K2 <- KeyPad, P1 /= start, P2 /= start
    }.

calculate_cost(Start, End, KeyPad, Costs) ->
    {no_path, Visited} = aoc_graph:dijkstra(
        {Start, $A}, fun(_) -> false end, neighbours(KeyPad, Costs)
    ),
    lists:min([Cost + maps:get([L, $A], Costs) || {E, L} := {Cost, _} <- Visited, E == End]).

neighbours(KeyPad, Costs) ->
    fun({Pos, Last}) ->
        Moves = [{M, move(Pos, M)} || M <- "^v<>"],
        [{maps:get([Last, M], Costs), {P, M}} || {M, P} <- Moves, maps:is_key(P, KeyPad)]
    end.

me_costs() ->
    Keys = maps:values(keypad_controller()),
    #{[K1, K2] => 1 || K1 <- Keys, K2 <- Keys}.
