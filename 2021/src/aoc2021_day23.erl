-module(aoc2021_day23).

-export([run/2, profile/3, eprof/2]).

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

profile(Star, File, Times) ->
    Data = read(File),
    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1({Start, Goal}) ->
   %%  maps:from_list(ends()).
    

    %% [{Pos, maps:get(Pos, Goal)} || Pos <- valid()].

    bfs([{0,{0,Start}}], Goal).

star2({Start, Goal}) ->
    dfs($D, Start, Goal).

    %% tools:print_grid(Start),
    %% io:format("~n~n"),
    %% tools:print_grid(Goal),
    
    %% tools:print_grid(map_of($A, Start)),
    %% io:format("~n~n"),
    %% tools:print_grid(map_of($B, Start)),
    %% io:format("~n~n"),

    %% tools:print_grid(map_of($C, Start)),
    %% io:format("~n~n"),
    %% tools:print_grid(map_of($D, Start)),
    %% io:format("~n~n").

read(File) ->
    erase(),

    Goal = "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########",
    {Valid, _} = lists:unzip(neigbours()), 

    {maps:with(Valid, tools:read_grid(File)), maps:with( Valid, tools:parse_grid(Goal))}.

bfs([ {_Estimate, {Cost, Goal}}], Goal) ->
    tools:print_grid(Goal),
    io:format("~n~p~n", [Cost]),
    Cost;
bfs([{_Estimate, {Cost, State}} | Rest], Goal) ->
    %% case get(State) of
    %% 	undefined  ->
    %% 	    put(State, visited),
    %% 	   %%  tools:print_grid(State),
    %% 	   %%  io:format("~n~p~n", [Cost]),
    %% 	    bfs(lists:umerge(Rest, next(Hd)), Goal);
    %% 	visited ->
    %% 	    bfs(Rest, Goal)
    %% end.
    bfs(lists:umerge(Rest, next({Cost, State}, Goal)), Goal).	

    %% %% tools:print_grid(First),
    %% %% io:format("~n~p~n", [Cost]),

%% bfs(lists:umerge(Rest, next(Hd)), Goal).

next({Cost, State}, Goal) ->
    Moves = [{Start, End, Type} || {Start, Neigbours} <- neigbours(),
				   (Type = maps:get(Start, State)) /= $., may_move(Start, Type, State),

				   End <-  valid_moves(Neigbours, Type, State)],

    Next = [{Cost + cost(Start, End, Type), 
			 State#{Start => $., End => Type}}
			|| {Start, End, Type} <- Moves],


    WCost = [begin
	 put(S, visited),
	 
	 
	 {C + dfs($A, S, Goal) + dfs($B, S, Goal) +dfs($C, S, Goal) + dfs($D, S, Goal)  , V}
	     end || V = {C ,S} <- Next, get(S) == undefined],
    
    lists:usort(WCost).





    %% %    maps:find
    %% [{Cost, Goal}].

cost({X1, Y1}, {X2, Y2}, T) ->
    (abs(X1 - X2) + abs(Y1 - Y2)) * c(T).


cost({X1, Y1}, {X2, Y2}) ->
    (abs(X1 - X2) + abs(Y1 - Y2)).
    

c($A) ->
    1;
c($B) ->
    10;
c($C) ->
    100;
c($D) ->
    1000.





%% #############
%% #12.4.6.8.01#
%% ###B#C#B#D###
%%   #A#D#C#A#
%%   #########

neigbours() ->
    [{{1, 1}, [{2, 1}]},
     {{2, 1}, [{1, 1}, {4, 1}, {3, 2}]},
     {{4, 1}, [{2, 1}, {6, 1}, {3, 2}, {5, 2}]},
     {{6, 1}, [{4, 1}, {8, 1}, {5, 2}, {7, 2}]},
     {{8, 1}, [{6, 1}, {10, 1}, {7, 2}, {9, 2}]},
     {{10, 1}, [{8, 1}, {11, 1}, {9, 2}]},
     {{11, 1}, [{10, 1}]},
     {{3, 2}, [{2, 1}, {4, 1}, {3, 3}]},
     {{3, 3}, [{3, 2}]},
     {{5, 2}, [{4, 1}, {6, 1}, {5, 3}]},
     {{5, 3}, [{5, 2}]},
     {{7, 2}, [{6, 1}, {8, 1}, {7, 3}]},
     {{7, 3}, [{7, 2}]},
     {{9, 2}, [{8, 1}, {10, 1}, {9, 3}]},
     {{9, 3}, [{9, 2}]}].


neigbours_posible() ->
    [{{1, 1}, [{2, 1}]},
     {{2, 1}, [{1, 1}, {4, 1}, {3, 2}]},
     {{4, 1}, [{2, 1}, {6, 1}, {3, 2}, {5, 2}]},
     {{6, 1}, [{4, 1}, {8, 1}, {5, 2}, {7, 2}]},
     {{8, 1}, [{6, 1}, {10, 1}, {7, 2}, {9, 2}]},
     {{10, 1}, [{8, 1}, {11, 1}, {9, 2}]},
     {{11, 1}, [{10, 1}]},
     {{3, 2}, [{2, 1}, {4, 1}, {3, 3}]},
     {{3, 3}, [{3, 2}]},
     {{5, 2}, [{4, 1}, {6, 1}, {5, 3}]},
     {{5, 3}, [{5, 2}]},
     {{7, 2}, [{6, 1}, {8, 1}, {7, 3}]},
     {{7, 3}, [{7, 2}]},
     {{9, 2}, [{8, 1}, {10, 1}, {9, 3}]},
     {{9, 3}, [{9, 2}]}].



ends() ->
    #{{3,2} => 65,
      {3,3} => 65,
      {5,2} => 66,
      {5,3} => 66,  
      {7,2} => 67,
      {7,3} => 67,
      {9,2} => 68,
      {9,3} => 68}.

may_move({_, 1}, _Type, _State) ->
    true;
may_move({X,3}, Type, _State) ->
    maps:get({X,3}, ends()) /= Type;
may_move({X,2}, Type, State) ->
    maps:get({X,2}, ends()) /= Type orelse maps:get({X,3}, State) /= Type.



    



valid_moves([Hd | Rest], Type,  State) ->
    lists:flatten(empty(Hd, State) ++ [empty(P, State) || P <- Rest, maps:get(P, ends(), Type) == Type]).
    


empty(Pos, State) ->
    case maps:get(Pos, State) of
	$. ->
	    [Pos];
	_ ->
	    []
    end.



map_of(Type, State) ->
    Map = maps:without([Type],  #{ $A => $.,
				   $B => $.,
				   $C => $.,
				   $D => $.}),
    tools:replace(State, Map , all).


dfs(Type, Start, Goal) ->
    c(Type)*dfs(map_of(Type, Start), map_of(Type, Goal)).

dfs(Goal, Goal) ->
    0;
dfs(Pos, Goal) ->
    case get(Pos) of


	undefined ->
	    put(Pos, visited),


	    Next = next_dfs(Pos),
	    Result = lists:min([dfs(N, Goal) +Dist  ||  {N, Dist} <- Next]),
	    if Result < 1000000 ->
		    put(Pos, Result);
	       true ->
		    erase(Pos)
	    end,
	    tools:print_grid(Pos),
	    io:format("~n~p~n", [Result]),

	    Result;
	visited ->
	    1000000;
	Value ->
	    Value
    end.

next_dfs(State) ->
    Moves = [{State#{Start => $., End => Type},  cost(Start, End)} || {Start, Neigbours} <- neigbours(), 
						 (Type = maps:get(Start, State)) /= $. , % , may_move(Start, Type, State),
						 

				    End <- valid_moves(Neigbours, Type, State)

	    ].
