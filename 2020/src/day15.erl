-module(day15).

-export([run/2]).

run(Star, File) ->
    Data = [8,11,0,19,1,2],
   %  Data = [0,3,6],

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
    TurnMap = maps:from_list(lists:zip(Data, lists:seq(1,length(Data)))),
    speek(2020, 0, length(Data) +1 , TurnMap).

    

star2(Data) ->
    ets:new(star2, [set, named_table]),
    [ets:insert(star2, Pair) || Pair <- lists:zip(Data, lists:seq(1,length(Data)))],
    speek_ets(30000000, 0, length(Data) +1 , star2).
    
    

speek(Goal, Last, Goal, TurnMap) ->
    Last;
speek(Goal, Last, Turn, TurnMap)->
    Speek = case maps:get(Last, TurnMap, 0) of
		0 ->
		    0;
		N ->
		    Turn - N

	    end,
    case Turn rem 1000000 of
	0 ->
	    io:format("Turn ~p: ~p -> ~p ~n",[Turn, Last,  Speek]);
	_ ->
	    ok
    end,
    
    speek(Goal, Speek, Turn+1, TurnMap#{Last => Turn}).

speek_ets(Goal, Last, Goal, Table) ->
    ets:delete(Table),
    Last;

speek_ets(Goal, Last, Turn, Table)->
    Speek = case ets:lookup(Table, Last) of
		[] ->
		    0;
		[{Last, N}] ->
		    Turn - N

	    end,
    case Turn rem 1000000 of
	0 ->
	    io:format("Turn ~p: ~p -> ~p ~n",[Turn, Last,  Speek]);
	_ ->
	    ok
    end,
    ets:insert(Table, {Last, Turn}),
    speek_ets(Goal, Speek, Turn+1, Table).
