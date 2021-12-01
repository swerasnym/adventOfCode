-module(aoc2019_day24).

-export([run/2, neigbours/1]).

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
    iterate(Data, sets:new()).

star2(Data) ->
    Iterations = 200,
    Map = map2(Data, Iterations),
    Map1 = iterate2(Map, Iterations),
    maps:fold(fun (_, bug, Acc) ->
                      Acc + 1;
                  (_, empty, Acc) ->
                      Acc
              end,
              0,
              Map1).

read(File) ->
    {ok, Bin} = file:read_file(File),
    string:split(
        string:trim(Bin), <<"\n">>, all),
    List = binary_to_list(string:trim(Bin)),
    read(List, 0, 0, #{}).

read([], _X, _Y, Acc) ->
    Acc;
read([$\n | Rest], _X, Y, Acc) ->
    read(Rest, 0, Y + 1, Acc);
read([Char | Rest], X, Y, Acc) ->
    case Char of
        $# ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => bug});
        $. ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => empty})
    end.

evolve(Map) ->
    maps:map(fun(Pos, Value) -> cell(Pos, Value, Map) end, Map).

iterate(Map, Set) ->
    Map1 = evolve(Map),
    Rating = rating(Map),
    case sets:is_element(Rating, Set) of
        true ->
            Rating;
        false ->
            iterate(Map1, sets:add_element(Rating, Set))
    end.

iterate2(Map, 0) ->
    Map;
iterate2(Map, N) ->
    iterate2(evolve(Map), N - 1).

cell(Pos, Value, Map) ->
    Neigbours = [N || N <- neigbours(Pos), maps:get(N, Map, empty) == bug],
    case {Value, length(Neigbours)} of
        {bug, 1} ->
            bug;
        {bug, _} ->
            empty;
        {empty, 1} ->
            bug;
        {empty, 2} ->
            bug;
        {empty, _} ->
            empty
    end.

rating(Map) ->
    maps:fold(fun(Pos, Value, Acc) ->
                 case {Pos, Value} of
                     {_, empty} -> 0;
                     {Pos, bug} -> value(Pos)
                 end
                 + Acc
              end,
              0,
              Map).

value({X, Y}) ->
    pow(2, X + 5 * Y).

pow(_, 0) ->
    1;
pow(A, 1) ->
    A;
pow(A, N) ->
    B = pow(A, N div 2),
    B
    * B
    * case N rem 2 of
          0 ->
              1;
          1 ->
              A
      end.

map2(Map, Iterations) ->
    Levels = (Iterations + 1) div 2,
    Map0 =
        maps:from_list([{{Level, X, Y}, empty}
                        || X <- lists:seq(0, 4),
                           Y <- lists:seq(0, 4),
                           Level <- lists:seq(-Levels, Levels)]),
    maps:fold(fun({X, Y}, Value, Acc) -> Acc#{{0, X, Y} => Value} end, Map0, Map).

neigbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}];
neigbours({_, 2, 2}) -> %? (middle)
    [];
neigbours({Level, X, Y}) ->
    Base = [{Level, X, Y - 1}, {Level, X, Y + 1}, {Level, X - 1, Y}, {Level, X + 1, Y}],

    Extra =
        case {X, Y} of
            {0, 0} ->
                [8, 12]; %a
            {0, 4} ->
                [12, 18]; %u
            {4, 0} ->
                [8, 14]; %e
            {4, 4} ->
                [14, 18]; %y
            {0, _} ->
                [12]; % f,k,p
            {4, _} ->
                [14]; % j,o,t
            {_, 0} ->
                [8]; % b,c,d
            {_, 4} ->
                [18];% v,w,x
            {2, 1} ->
                [a, b, c, d, e]; %h
            {1, 2} ->
                [a, f, k, p, u]; %l
            {3, 2} ->
                [e, j, o, t, y]; %n
            {2, 3} ->
                [u, v, w, x, y]; %r
            {_, _} ->
                []% g,i,s,q
        end,
    Base ++ lists:map(fun(Label) -> un_label(Label, Level) end, Extra).

un_label(Label, Level) ->
    case Label of
        a ->
            {Level + 1, 0, 0};
        b ->
            {Level + 1, 1, 0};
        c ->
            {Level + 1, 2, 0};
        d ->
            {Level + 1, 3, 0};
        e ->
            {Level + 1, 4, 0};
        f ->
            {Level + 1, 0, 1};
        j ->
            {Level + 1, 4, 1};
        k ->
            {Level + 1, 0, 2};
        o ->
            {Level + 1, 4, 2};
        p ->
            {Level + 1, 0, 3};
        t ->
            {Level + 1, 4, 3};
        u ->
            {Level + 1, 0, 4};
        v ->
            {Level + 1, 1, 4};
        w ->
            {Level + 1, 2, 4};
        x ->
            {Level + 1, 3, 4};
        y ->
            {Level + 1, 4, 4};
        8 ->
            {Level - 1, 2, 1};
        12 ->
            {Level - 1, 1, 2};
        14 ->
            {Level - 1, 3, 2};
        18 ->
            {Level - 1, 2, 3}
    end.
