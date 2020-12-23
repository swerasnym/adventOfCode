-module(day23).

-export([run/2]).

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

star1(Cups) ->
    move(100, Cups).

star2(Cups) ->
    Min = lists:max(Cups)+1,
    Max = 1000000,
    
    Result = move(4, Cups++lists:seq(Min,Max), Max).
    


read(example) ->
    [3,8,9,1,2,5,4,6,7];

read(_)->
    [3,6,4,2,8,9,7,1,5].






move(Times, Cups) ->
    move(Times, Cups, lists:max(Cups)).


move(0, Cups, _Max)->
    Cups;
move(Times,  [Current, P1, P2, P3| Rest], Max)->
    Pickup = [P1, P2, P3],
    Destination = destination(Current-1, Pickup, Max),
    move(Times-1, insert(Pickup, Destination, Rest, []) ++ [Current], Max).
    



destination(0, Pickup, Max) ->
    destination(Max,Pickup,  Max);
destination(Dest, [P1, P2, P3], _Max) when Dest /= P1,Dest /=P2, Dest /= P3 ->
    Dest;
destination(Dest, Pickup, Max) ->
    destination(Dest-1, Pickup, Max). 




insert(Pickup, Destination, [Destination| Rest], Previous) ->
    Previous ++ [Destination] ++ Pickup ++ Rest;
insert(Pickup, Destination, [First| Rest], Previous) ->
    insert(Pickup, Destination,  Rest, Previous++[First]).
