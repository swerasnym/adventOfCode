-module(day6).
-export([run/2]).

run(Star, File) ->
    Edges = read(File),

    G = digraph:new(),

    [begin
	 digraph:add_vertex(G, V1),
	 digraph:add_vertex(G, V2),
	 digraph:add_edge(G, V1, V2)
     end || {V1, V2} <- Edges ],
    case Star of
	star1 ->
	    star1(G);
	star2 ->
	    star2(G);
	_ ->
	    Star1 = star1(G),
	    Star2 = star2(G),
	    {Star1, Star2}
    end.

star1(G) ->
    depth(G, <<"COM">>).

depth(G, Vertex) ->
    depth(G, [{Vertex, 0}], 0).

depth(_, [], Acc) ->
    Acc;

depth(G, [{Vertex, Distance} | Rest], Acc) ->
    Neigbours = [{V, Distance +1} || V <- digraph:out_neighbours(G, Vertex)],
    depth(G, Neigbours ++ Rest, Acc + Distance).

star2(G) ->
    P1 = digraph:get_path(G, <<"COM">>, <<"YOU">>),
    P2 = digraph:get_path(G, <<"COM">>, <<"SAN">>),
    length(P1 -- P2) + length(P2 -- P1)  - 2.

read(File) ->
    {ok, Data} = file:read_file(File),
    List = string:split(string:trim(Data), "\n", all),

     [begin
		 [A,B] = string:split(Item, ")"),
		 {A,B}
	     end || Item <- List].
