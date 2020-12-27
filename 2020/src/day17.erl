-module(day17).

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

star1(Data) ->
    Grid = maps:from_list([{{X, Y, 0}, active} || {{X, Y}, active} <- maps:to_list(Data)]),
    maps:size(iterate(6, Grid)).

star2(Data) ->
    Grid = maps:from_list([{{X, Y, 0, 0}, active} || {{X, Y}, active} <- maps:to_list(Data)]),
    maps:size(iterate(6, Grid)).

read(File) ->
    tools:read_grid(File, #{$# => active, $. => inactive}).

iterate(0, Map) ->
    Map;
iterate(N, Map) ->
    Check =
        lists:usort(
            lists:flatmap(fun neigbours/1, maps:keys(Map))),

    Update =
        fun(Pos, Acc) ->
           ActiveNeigbours = [Neigbour || Neigbour <- neigbours(Pos), maps:is_key(Neigbour, Map)],

           case {maps:get(Pos, Map, inactive), length(ActiveNeigbours)} of
               {active, 2} -> Acc#{Pos => active};
               {active, 3} -> Acc#{Pos => active};
               {inactive, 3} -> Acc#{Pos => active};
               _ -> Acc
           end
        end,
    iterate(N - 1, lists:foldl(Update, #{}, Check)).

neigbours({X, Y, Z}) ->
    [{Xn, Yn, Zn}
     || Xn <- lists:seq(X - 1, X + 1),
        Yn <- lists:seq(Y - 1, Y + 1),
        Zn <- lists:seq(Z - 1, Z + 1),
        {Xn, Yn, Zn} /= {X, Y, Z}];
neigbours({X, Y, Z, W}) ->
    [{Xn, Yn, Zn, Wn}
     || Xn <- lists:seq(X - 1, X + 1),
        Yn <- lists:seq(Y - 1, Y + 1),
        Zn <- lists:seq(Z - 1, Z + 1),
        Wn <- lists:seq(W - 1, W + 1),
        {Xn, Yn, Zn, Wn} /= {X, Y, Z, W}].
