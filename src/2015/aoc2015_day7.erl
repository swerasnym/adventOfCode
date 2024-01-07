-module(aoc2015_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day7_ex.txt", star1, 65079}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Connections) ->
    Sort = top_sort(Connections),
    ResultMap = eval(Sort, maps:from_list(Connections), #{}),
    io:format("~kp~n", [ResultMap]),
    get(a, ResultMap).

star2(Connections) ->
    [b | WithoutB] = Sort = top_sort(Connections),
    ResultMap = eval(Sort, maps:from_list(Connections), #{}),
    A = get(a, ResultMap),
    ResultMap2 = eval(WithoutB, maps:from_list(Connections), #{b => A}),
    get(a, ResultMap2).

read(File) ->
    tools:read_lines(File, fun parse_connection/1).

parse_connection(String) ->
    [Expression, O] = string:split(String, " -> "),
    Output = parse_arg(O),
    case string:split(Expression, " ", all) of
        [A] ->
            {Output, {fun value/1, parse_args([A])}};
        ["NOT", A] ->
            {Output, {fun erlang:'bnot'/1, parse_args([A])}};
        [A, Op, B] ->
            {Output, {parse_op(Op), parse_args([A, B])}}
    end.

parse_op("AND") -> fun erlang:'band'/2;
parse_op("OR") -> fun erlang:'bor'/2;
parse_op("LSHIFT") -> fun erlang:'bsl'/2;
parse_op("RSHIFT") -> fun erlang:'bsr'/2.

value(V) -> V.

parse_args(Args) -> [parse_arg(Arg) || Arg <- Args].

parse_arg(Arg) ->
    try
        erlang:list_to_integer(Arg)
    catch
        _:_ ->
            erlang:list_to_atom(Arg)
    end.

top_sort(Connections) ->
    G = digraph:new(),
    [digraph:add_vertex(G, V) || {V, _} <- Connections],
    [digraph:add_edge(G, V2, V1) || {V1, {_, Es}} <- Connections, V2 <- Es, is_atom(V2)],
    Sort = digraph_utils:topsort(G),
    digraph:delete(G),
    Sort.

eval([], _ConMap, ResultMap) ->
    ResultMap;
eval([Key | Rest], ConMap, ResultMap) ->
    {Fun, Args} = maps:get(Key, ConMap),
    Result = erlang:apply(Fun, [get(A, ResultMap) || A <- Args]),
    eval(Rest, ConMap, ResultMap#{Key => tools:mod(Result, 65536)}).

get(Key, Map) when is_atom(Key) ->
    maps:get(Key, Map);
get(Int, _) when is_integer(Int) ->
    Int.
