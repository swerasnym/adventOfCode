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
    Count = count(lists:flatten([Edges || {_Id, Edges } <- Images])),
    lists:max(maps:values(Count)),
%    [V || V <- maps:values(Count), V == 1  ]


    product([Id || {Id, Edges } <- Images, is_corner(Edges, Count)]).
    
star2(Data) ->
    Data.


product(List) ->
    product(List, 1).

product([A], Result) ->
    A * Result;
product([A | Rest], Result) ->
    product(Rest, Result * A).


is_corner(Edges, Map) ->
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
            read_image(Rest, X + 1, Y, Acc#{{X, Y} => $0})
   end.

process("Tile "++ImageMeta)->
    {Id, ":\n"++Image} = string:to_integer(ImageMeta),
    ImageMap = read_image(Image),
    Edges = get_edges(ImageMap),
    {Id, Edges}.
    
count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).

get_edges(ImageMap = #{dim := {Xdim, Ydim}}) ->

    [
     _Top = list_to_integer([maps:get({X,0}, ImageMap) || X <- lists:seq(0, Xdim-1) ] ,2),
     _TopRev = list_to_integer([maps:get({X,0}, ImageMap) || X <- lists:seq(Xdim-1, 0, -1) ] ,2),
     
     _Bot = list_to_integer([maps:get({X,Ydim-1}, ImageMap) || X <- lists:seq(0, Xdim-1) ] ,2),
     _BotRev = list_to_integer([maps:get({X,Ydim-1}, ImageMap) || X <- lists:seq(Xdim-1, 0, -1) ] ,2),



     _Left = list_to_integer([maps:get({0,Y}, ImageMap) || Y <- lists:seq(0, Ydim-1) ] ,2),
     _LeftRev = list_to_integer([maps:get({0,Y}, ImageMap) || Y <- lists:seq(Ydim-1, 0, -1) ] ,2),
     
     _Right = list_to_integer([maps:get({Xdim-1,Y}, ImageMap) || Y <- lists:seq(0, Ydim-1) ] ,2),
     _RightRev = list_to_integer([maps:get({Xdim-1, Y}, ImageMap) || Y <- lists:seq(Ydim-1, 0, -1) ] ,2)
    

    ].

