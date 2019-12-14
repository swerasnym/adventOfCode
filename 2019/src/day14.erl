-module(day14).
-export([run/2, read/1]).

run(Star, File) ->
     Data = read(File),
    
    case Star of
	star1 ->
	    star1(Data);
	star2 ->
	    star2(Data);
	_ ->
	    Star1 = star1(Data),
	    Star2 = star2(Data),
	    {Star1, Star2}
    end.

star1(Data) -> 
    G =  build_graph(Data),
    
    Order = lists:droplast(digraph_utils:topsort(G)),

     
    F = fun (Result) -> 
		react(Result, Data)
	end,
    Reactions = lists:map(F, Order),
    Total = lists:foldl(fun do_react/2, #{"FUEL" => 1}, Reactions),
    maps:get("ORE", Total).

star2(Data) ->
    G =  build_graph(Data),
    
    Order = lists:droplast(digraph_utils:topsort(G)),

     
    F = fun (Result) -> 
		react(Result, Data)
	end,
    Reactions = lists:map(F, Order),
    
    Produce = fun(Fuel) ->
		      maps:get("ORE", lists:foldl(fun do_react/2, #{"FUEL" => Fuel}, Reactions))
	      end,

    Start = 1000000000000 div Produce(1),
    search(Start, 1000000000000, Produce).  
    
search(Start, Start, _)  ->
    Start;

search(Start, End, F) ->  
    Middle = (Start + End +1) div 2,
    case F(Middle) of
	Value when Value > 1000000000000 ->
	    search(Start, Middle - 1, F);
	_ ->
	    search(Middle, End, F)
    end.




read(File) ->
    {ok, Device} = file:open(File, [read]),
    read(Device, []).

read(Device, Acc) ->
    case file:read_line(Device) of
	eof ->
	    lists:sort(Acc);
	{ok, Line0} ->
	    Line = string:trim(Line0),
	    [Inputs, Output] = string:split(Line, " => " ),
	    InputsList = string:split(Inputs, ", ", all),
	    F = fun(Elem) ->  
			[Is, Element] = string:split(Elem, " " ),
			{Element, list_to_integer(Is)}
		end,
	    
	    read(Device, [{F(Output),  lists:map(F, InputsList)}|  Acc])   
	    

    end.    




build_graph(Data) ->
    G = digraph:new(),
    [build_graph(G, Elem) || Elem <- Data],
    G.
    
    
build_graph(G, {{Head, _}, List}) ->
	 digraph:add_vertex(G, Head),    
    [begin
	 digraph:add_vertex(G,Elem),
	 digraph:add_edge(G, Head, Elem)
     end|| {Elem, _} <- List].


react(Result, Data) ->
    F = fun ({{R, _},_}) -> 
		R == Result
	end,

    [React] = lists:filter(F, Data),
    React.


do_react({{Reaction, Output}, List} , Amounts) ->
    Amount = maps:get(Reaction, Amounts, 0),
    No = divup(Amount,Output),
    F = fun ({E,A}, Acc) ->
		C = maps:get(E, Acc, 0),	
		Acc#{E => C+ A*No}
	end,
    lists:foldl(F, Amounts, List).

					    
					    

divup(A,B) ->
    case A rem B of
	0 ->
	    A div B;
	_ ->
	    1 + A div B
    end.
		  




		
