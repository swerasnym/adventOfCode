-module(day12).
-export([run/2]).


-record(moon, {
	       pos = {0,0,0},
	       vel = {0,0,0}
	      }).


run(Star, File) ->
    {ok, Bin} = file:read_file(File),
    Data = string:trim(Bin),
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

star1(_Data) -> 
    lists:foldl(fun step/2, {0, data()},  lists:seq(1,1000)).
%    step({0, test()}).
    
		   


%    Data.
    

star2(_Data) ->
    X = step2(data(), x),
    Y = step2(data(), y),
    Z = step2(data(), z),
    {X,Y,Z, lcm(X, lcm(Y,Z))}.
    


step(_Count, Value) ->
    step(Value).

step({_Energy, Data0}) ->
    Data1 = velocity(Data0), 
    Data2 = position(Data1),
    {energy(Data2), Data2}.



step2(Data0, Axis) ->

    F = fun (_Key, Value) ->
		get_axis(Axis, Value)
	end,

    Data = maps:map(F, Data0),
    History = sets:add_element(Data, sets:new()),
    step2(0, Data, History).


step2(Step, Data0, History) ->
    Data1 = velocity(Data0), 
    Data2 = position(Data1),
    case sets:is_element(Data2, History) of
	true -> 
	    Step+1;
	false ->
	    step2(Step+1, Data2, sets:add_element(Data2, History))
    end.
						 
velocity(Data) ->
    lists:foldl(fun velocity/2, Data, pairs()).

velocity({M1, M2}, Data) ->
    #moon{pos ={X1,   Y1,  Z1}, vel={Vx, Vy, Vz}} = Moon = maps:get(M1, Data),
    #moon{pos ={X2,   Y2,  Z2}} = maps:get(M2, Data),
    Dx = velocity(X1, X2),
    Dy = velocity(Y1, Y2),
    Dz = velocity(Z1, Z2),
    Data#{M1 => Moon#moon{vel={Vx+Dx, Vy+Dy, Vz+Dz}}};

velocity(X1, X2) ->
    case X1 - X2 of
	     0 -> 0;
	     Diff when Diff > 0 -> -1;
	     Diff when Diff < 0 -> 1
    end.
    

position(Data) ->
    lists:foldl(fun position/2, Data, [1,2,3,4]).
    

position(M, Data) ->
    Moon = maps:get(M, Data),
    #moon{pos = {X,Y,Z}, vel={Dx,Dy,Dz}} = Moon, 
    Data#{M => Moon#moon{pos={X+Dx, Y+Dy, Z+Dz}}}.

test() ->
    #{1 => #moon{pos ={-1,   0,  2}},
      2 => #moon{pos ={ 2, -10, -7}},
      3 => #moon{pos ={ 4,  -8,  8}},
      4 => #moon{pos ={ 3,   5, -1}}
    }.

data() ->
    #{1 => #moon{pos ={13,   -13,  -2}},
      2 => #moon{pos ={16, 2, -15}},
      3 => #moon{pos ={7,  -18,  -12}},
      4 => #moon{pos ={-3,  -8, -8}}
    }.


get_axis(all, Moon) ->
    Moon;
get_axis(x, #moon{pos ={X,   _Y,  _Z}, vel={Vx, _Vy, _Vz}}) ->
    #moon{pos ={X, 0, 0}, vel={Vx, 0, 0}};
get_axis(y, #moon{pos ={_X,   Y,  _Z}, vel={_Vx, Vy, _Vz}}) ->
     #moon{pos ={0, Y, 0}, vel={0, Vy, 0}};
get_axis(z, #moon{pos ={_X,   _Y,  Z}, vel={_Vx, _Vy, Vz}}) ->
    #moon{pos ={0, 0, Z}, vel={0, 0, Vz}}.
    



pairs() ->
    List = [1,2,3,4],
    [{X,Y} || X <- List, Y <- List --[X]].    



energy(Data) when is_map(Data) ->
    lists:sum(lists:map(fun energy/1, maps:values(Data)));


energy(#moon{pos ={X,   Y,  Z}, vel={Vx, Vy, Vz}} ) ->
    (abs(X) + abs(Y) + abs(Z)) * (abs(Vx) + abs(Vy) + abs(Vz)).



gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).


lcm(A,B) ->
    A*B div gcd(A,B).
