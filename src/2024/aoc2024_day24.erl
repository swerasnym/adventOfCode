-module(aoc2024_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day24_ex.txt", star1, 2024}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Init, Gates}) ->
    Res = calculate(Gates, Init),
    value(Res, $z).

star2({Init, Gates}) ->
    Vars = maps:from_list(lists:flatten([classify_var(G) || G <- Gates])),
    Classified = [classify_gate(G, Vars) || G <- Gates],
    Problem =
        lists:flatten(
            [vars(G) || G <- Classified, problem_gate(G)] ++
                [V || V <- maps:values(Vars), problem_var(V)]
        ),
    Maps = lists:flatten(get_maps(lists:usort(Problem))),
    io:format("Searching ~p possibilities!~n", [length(Maps)]),
    X = value(Init, $x),
    Y = value(Init, $y),
    check(Init, X, Y, Gates, Maps).

read(File) ->
    [Init, Gates] = tools:read_blocks(File),
    {
        maps:from_list(tools:parse_lines(Init, fun parse_init/1)),
        tools:parse_lines(Gates, fun parse_gates/1)
    }.

parse_init(L) ->
    [Key, Value] = string:split(L, ": "),
    {Key, list_to_integer(Value)}.

parse_gates(L) ->
    [Key1, Op, Key2, "->", Res] = string:split(L, " ", all),
    [K1, K2] = lists:sort([Key1, Key2]),
    {K1, Op, K2, Res}.

value(no_solution, _) ->
    -1;
value(Map, Letter) ->
    Binary = [V + $0 || [L | _] := V <- maps:iterator(Map, ordered), L == Letter],
    list_to_integer(lists:reverse(Binary), 2).

calculate(Gates, Init) ->
    calculate(Gates, [], Init, length(Gates)).

calculate([], [], Map, _) ->
    Map;
calculate([], Skipped, Map, N) when N > length(Skipped) ->
    calculate(Skipped, [], Map, length(Skipped));
calculate([], _Skipped, _Map, _N) ->
    no_solution;
calculate([{Key1, Op, Key2, Res} = K | Rest], Skipped, Map, N) ->
    case {maps:get(Key1, Map, missing), maps:get(Key2, Map, missing)} of
        {missing, _} ->
            calculate(Rest, [K | Skipped], Map, N);
        {_, missing} ->
            calculate(Rest, [K | Skipped], Map, N);
        {V1, V2} ->
            calculate(Rest, Skipped, Map#{Res => op(Op, V1, V2)}, N)
    end.

op("XOR", V1, V2) ->
    V1 bxor V2;
op("AND", V1, V2) ->
    V1 band V2;
op("OR", V1, V2) ->
    V1 bor V2.
swap({A, Op, B, Res}, Map) ->
    {A, Op, B, maps:get(Res, Map, Res)}.

classify_var({_, "OR", _, Out}) ->
    {Out, {carry, Out}};
classify_var({"x" ++ _ = X, "XOR", "y" ++ _ = Y, Out}) ->
    [{X, {in, X}}, {Y, {in, Y}}, {Out, {sum, Out}}];
classify_var({_, "XOR", _, Out}) ->
    {Out, {out, Out}};
classify_var({"x00", "AND", "y00", Out}) ->
    {Out, {carry, Out}};
classify_var({_, "AND", _, Out}) ->
    {Out, {internal, Out}}.

classify_gate({A, Op, B, Res}, Map) ->
    [Ao, Bo] = lists:sort([maps:get(A, Map), maps:get(B, Map)]),
    {Ao, Op, Bo, maps:get(Res, Map)}.

problem_gate({{in, _}, "XOR", {in, _}, {sum, _}}) -> false;
problem_gate({{carry, _}, "XOR", {sum, _}, {out, _}}) -> false;
problem_gate({{carry, _}, "AND", {sum, _}, {internal, _}}) -> false;
problem_gate({{in, _}, "AND", {in, _}, {internal, _}}) -> false;
problem_gate({{in, "x00"}, "AND", {in, "y00"}, {carry, _}}) -> false;
problem_gate({{internal, _}, "OR", {internal, _}, {carry, _}}) -> false;
problem_gate(_) -> true.

vars({A, _, B, C}) -> [A, B, C].

problem_var({carry, "z45"}) -> false;
problem_var({sum, "z00"}) -> false;
problem_var({Type, "z" ++ _}) -> Type /= out;
problem_var({in, "x" ++ _}) -> false;
problem_var({in, "y" ++ _}) -> false;
problem_var({in, _}) -> true;
problem_var(_) -> false.

get_maps(Problem) ->
    {Pv, Rest0} = lists:partition(fun problem_var/1, Problem),
    Rest = [R || {_, L} = R <- Rest0, hd(L) /= $z],
    get_maps(4, Pv, Rest, #{}).

get_maps(0, [], _, Map) ->
    Map;
get_maps(N, [], Rest, Map) ->
    [get_maps(N, [V], Rest -- [V], Map) || V <- Rest];
get_maps(N, [{_, "z" ++ _ = LP} | RP], Rest, Map) ->
    [get_maps(N - 1, RP, Rest -- [V], Map#{LP => LV, LV => LP}) || {out, LV} = V <- Rest];
get_maps(N, [{_, LP} | RP], Rest, Map) ->
    [get_maps(N - 1, RP, Rest -- [V], Map#{LP => LV, LV => LP}) || {_, LV} = V <- Rest].

check(Init, X, Y, Gates0, [Swap | Rest]) ->
    Gates = [swap(Op, Swap) || Op <- Gates0],

    Res = calculate(Gates, Init),
    Z = value(Res, $z),
    case Z == X + Y of
        true ->
            string:join(lists:usort(maps:keys(Swap)), ",");
        false ->
            check(Init, X, Y, Gates0, Rest)
    end.
