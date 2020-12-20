-module(day20).

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

star1(Images) ->
    Count = count(lists:flatten([maps:values(Edges) || {_Id, {Edges, _Image} } <- Images])),
    product([Id || {Id, Edges, _Image } <- Images, is_corner(maps:values(Edges), Count)]).
    
star2(Images) ->

%% Count total #(1s) that are not part of a sea monster...

    Count = count(lists:flatten([maps:values(Edges) || {_Id, {Edges, _Image} } <- Images])),
    Counrers = [Id || {Id, {Edges, _Image} } <- Images, is_corner(maps:values(Edges), Count)],
    Sides = lists:flatten([reorder(Edges, Id) ||  {Id, {Edges, _Image}} <- Images]),
    


    {FirstEdges,_FirstImage} = proplists:get_value(hd(Counrers), Images),

    Internal = [Side || {Side, Value} <- maps:to_list(FirstEdges), maps:get(Value, Count) == 1, Side /= topRev, Side /= botRev  , Side /= leftRev , Side /= rightRev],
   
%    print( hd(Counrers) , Images).
    
    Neigbours =  build(Internal, hd(Counrers), Images, Sides, Count),
    Grid =  orient(Neigbours, Images, Sides, Count),

    Map = commbind(Grid),
    Monsters = count_mosters(get_tansforms(Map), read_image(make_moster())),
    #{$1 := MaybyMonster} = count(maps:values(Map)),
    MaybyMonster - 15* Monsters.



count_mosters([], _Monster)->
    0;

count_mosters([Map = #{dim := {Xdim, Ydim}} | Maps], Monster =  #{dim := {Mx, My}}) ->
    Positions = [{X,Y} || X <- lists:seq(0, Xdim-Mx -1), Y <- lists:seq(0, Ydim-My -1) , is_moster({X,Y}, Map, Monster)  ],

    case length(Positions) of 
        0 ->
            count_mosters(Maps, Monster);
        N ->
            print(highlight(Positions, Monster, Map)),
            N
    end.
            

highlight([], _Monster, Map) ->
    Map;
highlight([{X,Y}|Positions], Monster, Map) ->
    Highlights=     maps:from_list([{{X+Mx, Y+My}, $O} || {Mx, My} <- maps:keys(Monster)]),
    highlight(Positions, Monster, maps:merge(Map, Highlights)).



is_moster({X,Y}, Grid, Monster) ->
    Match =  [$# || {Mx, My} <- maps:keys(Monster), maps:get({X+Mx, Y+My}, Grid) == $1],
    length(Match) == maps:size(Monster)-1.
    
make_moster() ->
    ""++
        "                  #\n"++ 
        "#    ##    ##    ###\n" ++
        " #  #  #  #  #  #   ".





commbind(Grid = #{{0,0} := #{dim := {Xdim, Ydim}}}) ->

    List = [{{Gx*(Xdim -2) + X-1, Gy*(Ydim-2)+ Y-1}, Value } || {{Gx,Gy}, Image} <- maps:to_list(Grid), 
                                                                {{X,Y}, Value} <- maps:to_list(Image),
                                                                X /=0, Y/=0, X/=Xdim-1, Y/=Xdim-1],
    Map = maps:from_list(List),
    {Mx, My} = lists:max(maps:keys(Map)),
    Map#{dim => {Mx+1, My +1}}.


orient(Neigbours, Images, Sides, Count)->
    Grid = to_grid(Neigbours),
    {Gx, Gy} = lists:max(maps:keys(Grid)),
    orient({0,0},Grid , Images, Sides, Count, #{},{Gx+1, Gy+1}).




    
orient({_X,Ymax}, _Grid, _Images, _Sides, _Count, Acc, {_Xmax, Ymax}) ->
    
    Acc;
orient({Xmax,Y}, Grid, Images, Sides, Count, Acc, {Xmax, Ymax}) ->
    orient({0,Y+1}, Grid, Images, Sides, Count, Acc, {Xmax, Ymax});

orient(Pos = {X,Y}, Grid, Images, Sides, Count, Acc, Max) ->
    Id = maps:get(Pos, Grid),

    {_Edges, Map} =  proplists:get_value(Id, Images),
    [NewMap] = [Orientation || Orientation <- get_tansforms(Map), check_neigbours(Id, Pos, Orientation, Grid, Sides, Count)],
    
    orient({X+1,Y}, Grid, Images, Sides, Count, Acc#{Pos => NewMap}, Max).
    
    







check_neigbours(Id, {X,Y}, Map, Grid, Sides, Count) ->
    is_neigbour(Id, top(Map), maps:get({X,Y-1}, Grid, edge), Sides, Count) and
    is_neigbour(Id, bot(Map), maps:get({X,Y+1}, Grid, edge), Sides, Count) and
    is_neigbour(Id, left(Map), maps:get({X-1,Y}, Grid, edge), Sides, Count) and
    is_neigbour(Id, right(Map), maps:get({X+1,Y}, Grid, edge), Sides, Count).


get_tansforms(Map)->
    [Map, rotate(Map), rotate(rotate(Map)), rotate(rotate(rotate(Map))), 
     flip_x(Map), rotate(flip_x(Map)), rotate(rotate(flip_x(Map))), rotate(rotate(rotate(flip_x(Map)))) ].



is_neigbour(_Id, Value, edge, _Sides, Count) ->
    maps:get(Value,Count) == 1;

is_neigbour(Id, Value, Neigbour, Sides, _Count) ->
    case  [N || {N, _} <- proplists:get_all_values(Value, Sides), N /= Id] of
        [Neigbour] -> 
            
            true;
        _ ->
            false
    end.





to_grid(List) ->
    to_grid(List,0, #{}).

to_grid([], _Y, Map) ->
    Map;
to_grid([First| Rest], Y, Map) ->
    to_grid(Rest, Y+1, to_grid(First,  0, Y, Map)).

to_grid([],  _X, _Y, Map) ->
    Map;
to_grid([{Id, _Dir}| Rest],X,  Y, Map) ->
    to_grid(Rest, X+1, Y, Map#{{X,Y} => Id }).






rotate(Map = #{dim := Dim ={Xdim, Ydim}} ) ->
    A = (Xdim) div 2,
    B = (Ydim) div 2,
    NewMap = maps:from_list([{{-(Y-B) + A -1, X }, Value}  || {{X,Y}, Value} <- maps:to_list(Map)]),
    NewMap#{dim => Dim}.

flip_x(Map = #{dim := Dim ={Xdim, _Ydim}} ) ->
    NewMap = maps:from_list([{{Xdim -1 - X, Y }, Value}  || {{X,Y}, Value} <- maps:to_list(Map)]),
    NewMap#{dim => Dim}.
  
    
pc($0) ->
    $.;
pc($1) ->
    $~;
pc(C) ->
    C.


print(Map = #{dim := {Xdim, Ydim}}) ->
    [
         io:format("~s~n", [[pc(maps:get({X,Y}, Map))  || X <- lists:seq(0,Xdim-1)]])
     || Y <- lists:seq(0,Ydim-1) ].









    

opposed(top)->
    bot;
opposed(bot) ->
    top;
opposed(left) ->
    right;
opposed(right) ->
    left;
opposed(topRev)->
    botRev;
opposed(botRev) ->
    topRev;
opposed(leftRev) ->
    rightRev;
opposed(rightRev) ->
    leftRev.


flip(top)->
    topRev;
flip(bot) ->
    botRev;
flip(left) ->
    leftRev;
flip(right) ->
    rightRev;
flip(topRev)->
    top;
flip(botRev) ->
    bot;
flip(leftRev) ->
    left;
flip(rightRev) ->
    right.


build([E1,_E2],Id, Images, Sides, Count) ->
    Row1 = match_edges(E1, Id,  Images, Sides, Count),

    [match_edges(get_new_internal_edge(proplists:get_value(RowId, Images), Count, Side),RowId,Images,Sides,Count) || {RowId, Side} <- Row1  ].
    







match_edges(Edge, Id, Images, Sides, Count) ->
    {Edges, _Image} = proplists:get_value(Id, Images),
    Value = maps:get(opposed(Edge), Edges),
    case [V || V = {Ids, _} <- proplists:get_all_values(Value, Sides),
           Ids /= Id] of
        [{NextId, NextEdge}] ->
            [{Id,Edge} | match_edges(NextEdge, NextId, Images, Sides, Count)];
        [] ->
            [{Id,Edge}]
    end.










get_new_internal_edge({Edges, _Image}, Count, Current) ->
     [S, _Srev]=  [Side || {Side, Value} <- maps:to_list(Edges), maps:get(Value, Count) == 1,
                    Side /= Current, Side /= flip(Current), opposed(Side) /= Current, opposed(Side) /= flip(Current)  ],
    S.

   

reorder(Map, Id) ->
    [{Val, {Id, Key}} || {Key, Val} <- maps:to_list(Map)].
    


product(List) ->
    product(List, 1).

product([A], Result) ->
    A * Result;
product([A | Rest], Result) ->
    product(Rest, Result * A).


is_corner(Edges, Map) ->
    % two edges flipped an not with no pairing
    4 == length([V|| V <- Edges, maps:get(V,Map) == 1 ]).


read(File) ->
    {ok, Bin} = file:read_file(File),
    Images = string:split(string:trim(binary_to_list(Bin)), "\n\n", all),
    [process(Image) || Image <- Images].




read_image(Image)->
    read_image(Image, 0, 0, #{}).
read_image([], X, Y, Acc) ->
    Acc#{dim => {X, Y + 1}};
read_image([$\n], X, Y, Acc) ->
    Acc#{dim => {X, Y + 1}};
read_image([$\n | Rest], _X, Y, Acc) ->
    read_image(Rest, 0, Y + 1, Acc);
read_image([Char | Rest], X, Y, Acc) ->
     case Char of
         $# ->
            read_image(Rest, X + 1, Y, Acc#{{X, Y} => $1});
         $. ->
             read_image(Rest, X + 1, Y, Acc#{{X, Y} => $0});
         $  ->
             read_image(Rest, X + 1, Y, Acc)
   end.

process("Tile "++ImageMeta)->
    {Id, ":\n"++Image} = string:to_integer(ImageMeta),
    ImageMap = read_image(Image),
    Edges = get_edges(ImageMap),
    {Id, {Edges, ImageMap}}.
    
count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).

get_edges(ImageMap = #{dim := {Xdim, Ydim}}) ->

    #{
     top => list_to_integer([maps:get({X,0}, ImageMap)  || X <- lists:seq(0, Xdim-1) ] ,2),
     topRev => list_to_integer([maps:get({X,0}, ImageMap)  || X <- lists:seq(Xdim-1, 0, -1) ] ,2),
     
     bot => list_to_integer([maps:get({X,Ydim-1}, ImageMap)  || X <- lists:seq(0, Xdim-1) ] ,2),
     botRev => list_to_integer([maps:get({X,Ydim-1}, ImageMap) || X <- lists:seq(Xdim-1, 0, -1) ] ,2),



     left => list_to_integer([maps:get({0,Y}, ImageMap) || Y <- lists:seq(0, Ydim-1) ] ,2),
     leftRev => list_to_integer([maps:get({0,Y}, ImageMap) || Y <- lists:seq(Ydim-1, 0, -1) ] ,2),
     
     right => list_to_integer([maps:get({Xdim-1,Y}, ImageMap) || Y <- lists:seq(0, Ydim-1) ] ,2),
     rightRev => list_to_integer([maps:get({Xdim-1, Y}, ImageMap) || Y <- lists:seq(Ydim-1, 0, -1) ] ,2)
    

    }.



top(ImageMap = #{dim := {Xdim, _Ydim}}) ->
    list_to_integer([maps:get({X,0}, ImageMap)  || X <- lists:seq(0, Xdim-1) ] ,2).


bot(ImageMap = #{dim := {Xdim, Ydim}}) ->
    list_to_integer([maps:get({X,Ydim-1}, ImageMap)  || X <- lists:seq(0, Xdim-1) ] ,2).


left(ImageMap = #{dim := {_Xdim, Ydim}}) ->
    list_to_integer([maps:get({0,Y}, ImageMap) || Y <- lists:seq(0, Ydim-1) ] ,2).

right(ImageMap = #{dim := {Xdim, Ydim}}) ->
    list_to_integer([maps:get({Xdim-1,Y}, ImageMap) || Y <- lists:seq(0, Ydim-1) ] ,2).
    

