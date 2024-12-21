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
    Me = "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A",
    R1 = simulate(Me, key_map2()),
    R2 = simulate(R1, key_map2()),
    "029A" = simulate(R2, key_map1()),

    R1Costs = calculate_cost(key_map2(), me_costs()),
    R2Costs = calculate_cost(key_map2(), R1Costs),
    R3Costs = calculate_cost(key_map1(), R2Costs),

    CodesCosts = [{Code, press($A, Code, R3Costs, 0)} || Code <- Codes],
    lists:sum([numeric(Code) * Cost || {Code, Cost} <- CodesCosts]).

% Start = {2, 3},
% io:format("~p~n", [Data]),
% unknown.

star2(Codes) ->
    % R1Costs = calculate_cost(key_map2(), me_costs()),
    % R2Costs = calculate_cost(key_map2(), R1Costs),
    R25Costs = nested_costs(25, me_costs()),
    R3Costs = calculate_cost(key_map1(), R25Costs),

    CodesCosts = [{Code, press($A, Code, R3Costs, 0)} || Code <- Codes],
    lists:sum([numeric(Code) * Cost || {Code, Cost} <- CodesCosts]).

read(File) ->
    tools:read_lines(File).

nested_costs(0, Costs) ->
    Costs;
nested_costs(N, Costs) ->
    nested_costs(N - 1, calculate_cost(key_map2(), Costs)).

press(_, "", _, C) ->
    C;
press(F, [T | Rest], Map, C) ->
    press(T, Rest, Map, C + maps:get([F, T], Map)).

numeric(Code) ->
    list_to_integer(lists:flatten(string:replace(Code, "A", "", all))).

%% erlfmt:ignore Make the layout visible
 key_map1() ->
    #{
        {0, 0} => $7, {1, 0} => $8, {2, 0} => $9,
        {0, 1} => $4, {1, 1} => $5, {2, 1} => $6,
        {0, 2} => $1, {1, 2} => $2, {2, 2} => $3,
                      {1, 3} => $0, {2, 3} => $A,
        start => {2, 3}
    }.

%% erlfmt:ignore Make the layout visible
key_map2() ->
    #{
                      {1, 0} => $^, {2, 0} => $A,
        {0, 1} => $<, {1, 1} => $v, {2, 1} => $>,
        start => {2,0}
    }.

% 029A: <A^A>^^AvvvA
% AP1: v<<A>>^A<A>AvA<^AA>A<vAAA>^A
% Me: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A

move(Pos, $^) ->
    aoc_vector:add(Pos, {0, -1});
move(Pos, $v) ->
    aoc_vector:add(Pos, {0, 1});
move(Pos, $>) ->
    aoc_vector:add(Pos, {1, 0});
move(Pos, $<) ->
    aoc_vector:add(Pos, {-1, 0}).

simulate(error, _) ->
    error;
simulate(Sequence, KeyMap = #{start := Start}) ->
    simulate(Start, Sequence, KeyMap, []).

simulate(_Pos, [], _KeyMap, Out) ->
    lists:reverse(Out);
simulate(Pos, [$A | Rest], KeyMap, Out) ->
    simulate(Pos, Rest, KeyMap, [maps:get(Pos, KeyMap) | Out]);
simulate(Pos, [D | Rest], KeyMap, Out) ->
    Next = move(Pos, D),
    case maps:is_key(Next, KeyMap) of
        true ->
            simulate(Next, Rest, KeyMap, Out);
        false ->
            error
    end.

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
    Keys = "^v<>A",
    #{[K1, K2] => 1 || K1 <- Keys, K2 <- Keys}.
