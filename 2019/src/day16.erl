-module(day16).
-export([run/2, pattern/2, phase/1]).

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
    Result = phases(100,Data),
    {First, _} = lists:split(8, Result),
    
    list_to_integer([C+$0 || C <- First]).


star2(Data) ->
    Result = phases(100,  lists:flatten(lists:duplicate(10000, Data))),
    {First, _} = lists:split(7, Data),
    Offset = list_to_integer([C+$0 || C <- First]),
    
    {_, Last} = lists:split(Offset, Result),
    {First2, _} = lists:split(8, Last),

    {list_to_integer([C+$0 || C <- First2]), Result}.


read(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(string:trim(Bin)),
    [C-$0 || C <- String].


phases(0, List)->
    List;
phases(N, List)->
    phases(N-1, phase(List)).


phase(List) ->
    [phase(List, Position) || Position <- lists:seq(1,length(List))].

phase(List, Position) ->
    Didgit = lists:sum([A*B || {A,B} <- lists:zip(List, pattern(Position, length(List)))]),
    abs(Didgit) rem 10.



pattern(Position, Length) -> 
    Base = [0, 1, 0, -1],
    Rep = lists:flatten([lists:duplicate(Position, D) || D <- Base]),
    
    case length(Rep) > Length of 
	true->
	    [_|Rest] = Rep,
	    {Pattern, _} = lists:split(Length, Rest),
	    Pattern;
	false ->
	    Repititions = Length div length(Rep) +1,
	    
	    [_|Rest] = lists:flatten(lists:duplicate(Repititions, Rep)),
	    {Pattern, _} = lists:split(Length, Rest),
	    Pattern
    end.
    
