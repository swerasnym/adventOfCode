-module(day6).
-export([run/2, read/1]).

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

    F = fun (V) ->
		{Com, _} = digraph:vertex(G, <<"COM">>),
		case digraph:get_path(G, Com, V) of
		    false ->
			0;
		    Path ->
			    length(Path) -1
		end
	end,

   
    lists:sum([F(V) || V <-  digraph:vertices(G)]). 
    


star2(G) ->
    {Com, _} = digraph:vertex(G, <<"COM">>),
    {You, _} = digraph:vertex(G, <<"YOU">>), 
    {San, _} = digraph:vertex(G, <<"SAN">>), 
    P1 = digraph:get_path(G, Com, You),
    P2 = digraph:get_path(G, Com, San),
    
    P3 = sets:to_list(sets:intersection(sets:from_list(P1), sets:from_list(P2))),

    length(P1) +length(P2) - 2* length(P3) -2 .



read(File) ->
    {ok, Data} = file:read_file(File),
    List = string:split(string:trim(Data), "\n", all),
	
     [begin
		 [A,B] = string:split(Item, ")"),
		 {A,B}
	     end || Item <- List].


