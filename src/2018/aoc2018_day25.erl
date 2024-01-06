-module(aoc2018_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day25_ex.txt", star1, 2}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Stars) ->
    Enumerated = lists:enumerate(Stars),
    Edges = [
        {A, B}
     || {A, PA} <- Enumerated, {B, PB} <- Enumerated, A /= B, aoc_vector:manhattan(PA, PB) =< 3
    ],
    G = build_graph(lists:seq(1, length(Stars)), Edges),
    length(digraph_utils:components(G)).

star2(_Data) ->
    {done, "Trigger time underflow with help from Rudolph!"}.

read(File) ->
    tools:read_lines(File, {fun tools:parse_integers/2, [[$,]]}).

build_graph(Vertices, Edges) ->
    G = digraph:new(),
    [digraph:add_vertex(G, V) || V <- Vertices],
    [digraph:add_edge(G, V1, V2) || {V1, V2} <- Edges],
    G.
