-module(day17).
-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    
    case Star of
	star1 ->
	    star1(Program);
	star2 ->
	    star2(Program);
	_ ->
	    Star1 = star1(Program),
	    Star2 = star2(Program),
	    {Star1, Star2}
    end.

star1(Program) ->
    Pid = intcode:spawn(Program, [{outputpid, self()} ,{inputpid, self()}]),
    {halt, View} = intcode:recvn(Pid, all),

   
    Map = scan(View),

    lists:sum([intersection(Pos, Map) || Pos <- maps:keys(Map)]).
    


star2(Program) ->
    

    Pid = intcode:spawn(Program, [{outputpid, self()} ,{inputpid, self()}]),
    {halt, View} = intcode:recvn(Pid, all),
    Map = scan(View),
    Path = path(Map),
    
    
    Input = lists:flatten("A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C\n" ++  split(Path)),


%% %A:R,10,L,12,R,6,         
%% AR,10,L,12,R,6,         
%% %B:R,6,R,10,R,12,R,6,     
%% %C:R,10,L,12,L,12,        
%% BR,6,R,10,R,12,R,6,     
%% CR,10,L,12,L,12,        
%% BR,6,R,10,R,12,R,6,     
%% CR,10,L,12,L,12,        
%% BR,6,R,10,R,12,R,6,     
%% AR,10,L,12,R,6,   


    Result = intcode:run(Program, [{set, 2,0}, {input, "A,A,B,C,B,C,B,C,B,A\nR,10,L,12,R,6\nR,6,R,10,R,12,R,6\nR,10,L,12,L,12\nN\n"}]),
    Output = intcode:get_output(Result),
    lists:last(Output).
    




% Pid = 




scan(View) ->
    io:format("~s",[View]),
    scan(View, 0, 0, #{}).
    
    
scan([], _, _, Acc) ->
    Acc;
scan([$.|Rest], X, Y, Acc) ->
    scan(Rest, X+1, Y, Acc);

scan([$\n|Rest], _, Y, Acc) ->
    scan(Rest, 0, Y+1, Acc);

scan([First|Rest], X, Y, Acc) ->

    Term = case First of 
	       $# ->
		   scaffold;
	       $^ ->
		   {robot, north};
	       $v ->
		    {robot, south};
	       $> ->
		    {robot, west};
	       $< ->
		   {robot, east}
	   end,
    scan(Rest, X+1, Y, Acc#{{X,Y} => Term}).
	
		   


intersection(Pos, Map) ->
    case neigbours(Pos, Map) of
	{scaffold,scaffold,scaffold,scaffold} ->
	    alignment(Pos);
	_ ->
	    0
    end.
	 
alignment({X,Y}) ->
    X*Y.

neigbours({X,Y} , Map) ->
    {maps:get({X,Y+1}, Map, empty), 
     maps:get({X,Y-1}, Map, empty), 
     maps:get({X+1,Y}, Map, empty), 
     maps:get({X-1,Y}, Map, empty)}.
			       


path(Map) ->
    [{Pos, {robot, Direction}}] = 
	maps:to_list(maps:filter(fun(_,V) ->
					case V of
					    {robot,_} -> 
						true;
					    _ -> 
						false 
					end 
				 end, Map)),
    
    path(Pos, Direction, Map, []).


path(Pos, Direction, Map, Acc) ->

    case {maps:get(move(Pos, turn(Direction, $R)), Map, none),
	  maps:get(move(Pos, turn(Direction, $L)), Map, none)} of
	{scaffold, _} ->
	    NewDir = turn(Direction, $R),
	    {Pos1, Distance} = find_end(Pos, NewDir, Map, 0),
	    Command = lists:concat(["R",",", Distance,","]),
	    
	    path(Pos1, NewDir, Map, [ Command | Acc]);
	{_, scaffold} -> 
	    	    NewDir = turn(Direction, $L),
	    {Pos1, Distance} = find_end(Pos, NewDir, Map, 0),
	    Command = lists:concat(["L",",", Distance,","]),
	    
	    path(Pos1, NewDir, Map, [ Command | Acc]);
	_ ->
	    
	    lists:droplast(lists:flatten(lists:reverse(Acc)))
    end.
	


find_end(Pos0, Dir, Map, N) ->
    Pos = move(Pos0, Dir),
    case maps:get(Pos, Map, none) of
	scaffold ->
	    find_end(Pos, Dir, Map, N+1);
	_ ->
	    {Pos0, N}
    end.
    


split(String) ->
    split(string:split(String, ",", all) ,[],[]).

split([], String,  Acc) ->
   lists:reverse([lists:flatten(String++"\n") | Acc]);
split([E|Es], [],  Acc) ->
    split(Es, E, Acc);
split([E|Es], String,  Acc) ->
    case length(String) + length(E) + 1 of
	Value when Value > 18 ->
	    split([E|Es], [],  [lists:flatten(String++ "\n") | Acc]);
	_ ->
	     split(Es, String ++ "," ++ E,  Acc)
    end.

    
    
    
	





turn(north, $L) ->
    west;
turn(north, $R) ->
    east;

turn(west, $L) ->
    south;
turn(west, $R) ->
    north;

turn(south, $L) ->
    east;
turn(south, $R) ->
    west;

turn(east, $L) ->
    north;
turn(east, $R) ->
    south.

move({X,Y}, north) ->
    {X, Y - 1};
move({X,Y}, south) ->
    {X, Y + 1};
move({X,Y}, east) ->
    {X + 1, Y};
move({X,Y}, west) ->
    {X - 1, Y}.


%% R,10,L,12,R,6, A
%% R,10,L,12,R,6, A
%% R,6,R,10,R,12,R,6,
%% R,10,L,12,L,12, 
%% R,6,R,10,R,12,R,6,
%% R,10,L,12,L,12,
%% R,6,R,10,R,12,R,6,
%% R,10,L,12,L,12,
%% R,6,R,10,R,12,R,6, 
%% R,10,L,12,R,6




%% AR,10,L,12,R,6,         
%% AR,10,L,12,R,6,         
%% BR,6,R,10,R,12,R,6,     
%% CR,10,L,12,L,12,        
%% BR,6,R,10,R,12,R,6,     
%% CR,10,L,12,L,12,        
%% BR,6,R,10,R,12,R,6,     
%% CR,10,L,12,L,12,        
%% BR,6,R,10,R,12,R,6,     
%% AR,10,L,12,R,6,         



%% AR,10,L,12,R,6,  
%% AR,10,L,12,R,6,         
%% AR,10,L,12,R,6,         
%% BR,6,R,10,R,12,R,6,     
%% BR,6,R,10,R,12,R,6,     
%% BR,6,R,10,R,12,R,6,     
%% BR,6,R,10,R,12,R,6,     
%% CR,10,L,12,L,12,        
%% CR,10,L,12,L,12,        
%% CR,10,L,12,L,12,         


%% R,10,L,12,L,12,
%% R,10,L,12,L,12,
%% R,10,L,12,L,12,
%% R,10,L,12,R,6,
%% R,10,L,12,R,6,
%% R,10,L,12,R,6,
%% R,6,R,10,R,12,R,6,
%% R,6,R,10,R,12,R,6,
%% R,6,R,10,R,12,R,6,
%% R,6,R,10,R,12,R,6,


