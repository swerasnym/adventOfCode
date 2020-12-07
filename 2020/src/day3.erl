-module(day3).

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

star1(Map) ->
    {_Xmax, Ymax} = maps:get(dim, Map),
    Results = [get_pos(Y * 3, Y, Map) || Y <- lists:seq(0, Ymax - 1)],
    maps:get(tree, count(Results)).

star2(Map) ->
    {_Xmax, Ymax} = maps:get(dim, Map),
    #{tree := T1} = count([get_pos(Y * 1, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T2} = count([get_pos(Y * 3, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T3} = count([get_pos(Y * 5, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T4} = count([get_pos(Y * 7, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T5} = count([get_pos(N, N * 2, Map) || N <- lists:seq(0, Ymax div 2)]),
    T1 * T2 * T3 * T4 * T5.

read(File) ->
    {ok, Bin} = file:read_file(File),
    List = binary_to_list(Bin),
    read(List, 0, 0, #{}).

read([$\n], X, Y, Acc) ->
    Acc#{dim => {X, Y + 1}};
read([$\n | Rest], _X, Y, Acc) ->
    read(Rest, 0, Y + 1, Acc);
read([Char | Rest], X, Y, Acc) ->
    case Char of
        $# ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => tree});
        $. ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => open})
    end.

get_pos(X, Y, #{dim := {Xmax, _Ymax}} = Map) ->
    maps:get({X rem Xmax, Y}, Map).

count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).
