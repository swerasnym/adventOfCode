-module(day24).
-export([run/2,neigbours/1]).

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
    iterate(Data,sets:new()).


star2(Data) ->
    Map = map2(Data),
    Map1 = iterate2(Map, 200),
    [begin paint(Level, Map1),io:nl() end || Level <- lists:seq(-6,6)],
    maps:fold(fun (_, bug, Acc ) -> Acc +1; (_, empty, Acc ) -> Acc end, 0, Map1).


read(File) ->
    {ok, Bin} = file:read_file(File),
    string:split(string:trim(Bin), <<"\n">>, all),
    List = binary_to_list(string:trim(Bin)),
    read(List, 0 ,0, #{}).


read([], _X, _Y, Acc) ->
    Acc;
read([$\n|Rest], _X, Y, Acc) ->
    read(Rest, 0, Y+1, Acc);
read([Char |Rest], X, Y, Acc) ->
    case Char of
	$# ->
	    read(Rest, X+1, Y, Acc#{{X,Y} => bug});
	$. ->
	    read(Rest, X+1, Y, Acc#{{X,Y} => empty})
    end.


evolve(Map) ->
    maps:map(fun (Pos,Value) -> cell(Pos,Value,Map) end, Map).

iterate(Map, Set) ->
    Map1 = evolve(Map),
    Rating = rating(Map),
    case sets:is_element(Rating,Set) of
	true ->
	    Rating;
	false ->
	    iterate(Map1, sets:add_element(Rating, Set))
    end.

iterate2(Map, 0) ->
    Map;
iterate2(Map, N) ->
    iterate2(evolve(Map), N-1).


cell(Pos, Value, Map) ->
    Neigbours = [A || A <- neigbours(Pos), maps:get(A,Map,empty) == bug],
    case {Value, length(Neigbours)} of
	{bug, 1} ->
	    bug;
	{bug,_} ->
	    empty;
	{empty, 1} ->
	    bug;
	{empty, 2} ->
	    bug;
	{empty, _} ->
	    empty
    end.



paint(Screen) ->
    Xs = [X || {X,_} <- maps:keys(Screen)],
    Ys = [Y || {_,Y} <- maps:keys(Screen)],


    [paint({X,Y}, Screen, lists:max(Xs)) || Y <- lists:seq(lists:min(Ys), lists:max(Ys)),
					  X <- lists:seq(lists:min(Xs), lists:max(Xs))],
    ok.

paint({X,_} = Pos, Screen, X) ->
    case maps:get(Pos, Screen,black) of
	empty ->
	    io:format(".~n",[]);
	bug->
	    io:format("#~n",[])
    end;
paint(Pos, Screen, _) ->
    case maps:get(Pos, Screen,black) of
	empty ->
	    io:format(".",[]);
	bug->
	    io:format("#",[])
    end.

paint(Level, Screen) ->
    io:format("Depth ~p:~n", [Level]),
    Xs = [X || {_,X,_} <- maps:keys(Screen)],
    Ys = [Y || {_,_,Y} <- maps:keys(Screen)],


    [paint(Level, {X,Y}, Screen, lists:max(Xs)) || Y <- lists:seq(lists:min(Ys), lists:max(Ys)),
						   X <- lists:seq(lists:min(Xs), lists:max(Xs))],
    ok.
paint(_Level, {2,2}, _Screen, _) ->
    io:format("?");
paint(Level, {X,Y}, Screen, X) ->
    case maps:get({Level,X,Y}, Screen,black) of
	empty ->
	    io:format(".~n",[]);
	bug->
	    io:format("#~n",[])
    end;
paint(Level,  {X,Y}, Screen, _) ->
    case maps:get({Level,X,Y}, Screen,black) of
	empty ->
	    io:format(".",[]);
	bug->
	    io:format("#",[])
    end.


rating(Map) ->
    maps:fold(fun (Pos, Value, Acc) ->
		      Acc +  case {Pos, Value} of
				 {_, empty} ->
				     0;
				 {Pos, bug} ->
				     io:format("~p ~p~n", [Pos, value(Pos)]),
				     value(Pos)
			     end
					 
	      end, 0 ,Map).

value(Pos) ->
    case Pos of
     {0,0} -> 1;
     {1,0} -> 2;
     {2,0} -> 4;
     {3,0} -> 8;
     {4,0} -> 16;

     {0,1} -> 32;
     {1,1} -> 64;
     {2,1} -> 128;
     {3,1} -> 256;
     {4,1} -> 512;

     {0,2} -> 1024;
     {1,2} -> 2048;
     {2,2} -> 4096;
     {3,2} -> 8192;
     {4,2} -> 16384;

     {0,3} -> 32768;
     {1,3} -> 65536;
     {2,3} -> 131072;  
     {3,3} -> 262144;  
     {4,3} -> 524288;  
     {0,4} -> 1048576; 
     {1,4} -> 2097152; 
     {2,4} -> 4194304; 
     {3,4} -> 8388608; 
     {4,4} -> 16777216
    end.




map2(Map) ->
    Map0 = maps:from_list([{{Z,X,Y},empty} || X <- lists:seq(0,4), Y <- lists:seq(0,4), Z <- lists:seq(-200,200)]),
    maps:fold(fun({X,Y}, Value, Acc) -> Acc#{{0,X,Y} => Value} end, Map0, Map).

    


neigbours({X,Y}) ->
    [{X, Y - 1},
     {X, Y + 1},
     {X - 1, Y},
     {X + 1, Y}];
neigbours({_, 2,2}) ->
    [];
neigbours({Level, X,Y}) ->
   Base = [{Level, X, Y - 1},
	   {Level,X, Y + 1},
	   {Level,X - 1, Y},
	   {Level, X + 1, Y}],
 


    Extra = case {X,Y} of
		{0,0} -> %a
		    [8,12];

		{0,4} -> %u
		    [12,18];
		{4,0} -> %e 
		    [8,14];

		{4,4} -> %y
		    [14,18];

		{0,_} -> % f,k,p
		    [12];

		{4,_} -> % j,o,t
		    [14];

		{_,0} -> % b,c,d
		    [8];

		{_,4} -> % v,w,x
		    [18];


		{2,1} -> %8
		    [a,b,c,d,e];
		{1,2} -> %12
		    [a,f,k,p,u];
		{3,2} -> %14
		    [e,j,o,t,y];
		{2,3} -> % 18
		    [u,v,w,x,y];
		{_,_} ->
		    []
	    end,
    Base ++ lists:map(fun(Label) -> un_label(Label, Level) end, Extra).


un_label(Label, Level) ->
    case Label of
	a    -> {Level+1,0,0};
	b    -> {Level+1,1,0};
	c    -> {Level+1,2,0};
	d    -> {Level+1,3,0};
	e    -> {Level+1,4,0};

	f    -> {Level+1,0,1};
%	g    -> {Level+1,1,1};
%	h    -> {Level+1,2,1};
%	i    -> {Level+1,3,1};
	j    -> {Level+1,4,1};

	k    -> {Level+1,0,2};
%	l    -> {Level+1,1,2};
	%m    -> {Level+1,2,2};
%	n    -> {Level+1,3,2};
	o    -> {Level+1,4,2};

	p    -> {Level+1,0,3};
%	q    -> {Level+1,1,3};
%	r    -> {Level+1,2,3};
%	s    -> {Level+1,3,3};
	t    -> {Level+1,4,3};

	u    -> {Level+1,0,4};
	v    -> {Level+1,1,4};
	w    -> {Level+1,2,4};
	x    -> {Level+1,3,4};
	y    -> {Level+1,4,4};

	%% 1     -> {Level-1,0,0};
	%% 2     -> {Level-1,1,0};
	%% 3     -> {Level-1,2,0};
	%% 4     -> {Level-1,3,0};
	%% 5     -> {Level-1,4,0};

	%% 6     -> {Level-1,0,1};
	%% 7     -> {Level-1,1,1};
	8     -> {Level-1,2,1};
	%% 9     -> {Level-1,3,1};
	%% 10    -> {Level-1,4,1};

	%% 11    -> {Level-1,0,2};
	12    -> {Level-1,1,2};
	%13    -> {Level-1,2,2};
	14    -> {Level-1,3,2};
	%% 15    -> {Level-1,4,2};

	%% 16    -> {Level-1,0,3};
	%% 17    -> {Level-1,1,3};
	18    -> {Level-1,2,3}
	%% 19    -> {Level-1,3,3};
	%% 20    -> {Level-1,4,3};

	%% 21    -> {Level-1,0,4};
	%% 22    -> {Level-1,1,4};
	%% 23    -> {Level-1,2,4};
	%% 24    -> {Level-1,3,4};
	%% 25    -> {Level-1,4,4}
    end.
