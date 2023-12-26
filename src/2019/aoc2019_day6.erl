-module(aoc2019_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(G) ->
    depth(G, <<"COM">>).

depth(G, Vertex) ->
    depth(G, [{Vertex, 0}], 0).

depth(_, [], Acc) ->
    Acc;
depth(G, [{Vertex, Distance} | Rest], Acc) ->
    Neigbours = [{V, Distance + 1} || V <- digraph:out_neighbours(G, Vertex)],
    depth(G, Neigbours ++ Rest, Acc + Distance).

star2(G) ->
    P1 = digraph:get_path(G, <<"COM">>, <<"YOU">>),
    P2 = digraph:get_path(G, <<"COM">>, <<"SAN">>),
    length(P1 -- P2) + length(P2 -- P1) - 2.

read(File) ->
    {ok, Data} = file:read_file(File),
    List = string:split(string:trim(Data), "\n", all),

    Edges = [
        begin
            [A, B] = string:split(Item, ")"),
            {A, B}
        end
     || Item <- List
    ],

    G = digraph:new(),

    [
        begin
            digraph:add_vertex(G, V1),
            digraph:add_vertex(G, V2),
            digraph:add_edge(G, V1, V2)
        end
     || {V1, V2} <- Edges
    ],
    G.
