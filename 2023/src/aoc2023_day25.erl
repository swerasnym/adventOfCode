-module(aoc2023_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day25_ex.txt", star1, 54}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({G, Edges0}) ->
    {_, Edges} = lists:unzip(Edges0),

    %% My edges to cut. Found manually by modifying my input, plotting with Graphviz neato algorithm.
    %% By adding these as the first edges to try the cutting will succeed at once :)
    Cut = [{"sdv", "mxv"}, {"klj", "scr"}, {"gqr", "vbk"}],
    Extra = [Ee || {E, Ee} <- Edges0, lists:member(E, Cut)],

    [Ca, Cb] = split(G, Extra ++ Edges),
    length(Ca) * length(Cb).

star2(_) ->
    {done, "Press the button: Let it snow!"}.

read(File) ->
    Graph = digraph:new(),
    Lines = tools:read_lines(File, fun parse_line/1),
    Map = maps:from_list(Lines),

    {From, To} = lists:unzip(Lines),
    Vs = lists:usort(lists:concat(To) ++ From),
    VertexMap = maps:from_list([{Key, digraph:add_vertex(Graph)} || Key <- Vs]),

    Edges = [
        [
            {{Key, N}, digraph:add_edge(Graph, maps:get(Key, VertexMap), maps:get(N, VertexMap))}
         || N <- maps:get(Key, Map)
        ]
     || Key <- maps:keys(Map)
    ],
    {Graph, lists:concat(Edges)}.

parse_line(L) ->
    [Label, Neigbours] = string:split(L, ": "),

    {Label, string:split(Neigbours, " ", all)}.

split(_, []) ->
    none;
split(G, [E | Rest]) ->
    io:format("A ~p~n", [length(Rest)]),
    case split(G, E, Rest) of
        none ->
            split(G, Rest);
        Res ->
            Res
    end.

split(_, _, []) ->
    none;
split(G, E1, [E2 | Rest]) ->
    case split(G, E1, E2, Rest) of
        none ->
            split(G, E1, Rest);
        Res ->
            Res
    end.

split(_, _, _, []) ->
    none;
split(G, E1, E2, [E3 | Rest]) ->
    {E1, V11, V12, _} = digraph:edge(G, E1),
    {E2, V21, V22, _} = digraph:edge(G, E2),
    {E3, V31, V32, _} = digraph:edge(G, E3),

    digraph:del_edge(G, E1),
    digraph:del_edge(G, E2),
    digraph:del_edge(G, E3),
    Comp = digraph_utils:components(G),
    case length(Comp) of
        2 ->
            Comp;
        1 ->
            E1 = digraph:add_edge(G, E1, V11, V12, []),
            E2 = digraph:add_edge(G, E2, V21, V22, []),
            E3 = digraph:add_edge(G, E3, V31, V32, []),
            split(G, E1, E2, Rest)
    end.
