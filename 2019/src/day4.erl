-module(day4).
-export([run/2]).

run(Star, _) ->
    Data = {183564,657474},

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

star1({To, From}) ->
    Codes0 = lists:map(fun integer_to_list/1,  lists:seq(To,From)),
    Codes1 = lists:filter(fun same/1, Codes0),
    Codes2 = lists:filter(fun nondec/1, Codes1),
    length(Codes2).

star2({To, From}) ->
    Codes0 = lists:map(fun integer_to_list/1,  lists:seq(To,From)),
    Codes1 = lists:filter(fun same2/1, Codes0),
    Codes2 = lists:filter(fun nondec/1, Codes1),
    length(Codes2).

same([A,A,_,_,_,_]) ->
    true;
same([_,A,A,_,_,_]) ->
    true;
same([_,_,A,A,_,_]) ->
    true;
same([_,_,_,A,A,_]) ->
    true;
same([_,_,_,_,A,A]) ->
    true;
same(_) ->
    false.

same2([A,A,B,_,_,_]) when B /= A ->
    true;
same2([C,A,A,B,_,_])  when B /= A andalso C /= A  ->
    true;
same2([_,C,A,A,B,_])  when B /= A andalso C /= A  ->
    true;
same2([_,_,C,A,A,B])  when B /= A andalso C /= A  ->
    true;
same2([_,_,_,C,A,A]) when C /= A ->
    true;
same2(_) ->
    false.

nondec([A|As]) ->
    nondec(As, A).

nondec([], _) ->
    true;
nondec([A|_], Prev)  when A < Prev ->
    false;
nondec([A |As],_) ->
    nondec(As, A).
