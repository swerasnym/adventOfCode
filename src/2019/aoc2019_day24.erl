-module(aoc2019_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    iterate(Data, sets:new()).

star2(Data) ->
    Iterations = 200,
    Map = map2(Data, Iterations),
    Map1 = iterate2(Map, Iterations),
    maps:fold(
        fun
            (_, bug, Acc) ->
                Acc + 1;
            (_, empty, Acc) ->
                Acc
        end,
        0,
        Map1
    ).

read(File) ->
    {ok, Bin} = file:read_file(File),
    string:split(string:trim(Bin), <<"\n">>, all),
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
    maps:fold(
        fun(Pos, Value, Acc) ->
            case {Pos, Value} of
                {_, empty} ->
                    0;
                {Pos, bug} ->
                    value(Pos)
            end +
                Acc
        end,
        0,
        Map
    ).

value({X, Y}) ->
    pow(2, X + 5 * Y).

pow(_, 0) ->
    1;
pow(A, 1) ->
    A;
pow(A, N) ->
    B = pow(A, N div 2),
    B *
        B *
        case N rem 2 of
            0 ->
                1;
            1 ->
                A
        end.

map2(Map, Iterations) ->
    Levels = (Iterations + 1) div 2,
    Map0 =
        maps:from_list([
            {{Level, X, Y}, empty}
         || X <- lists:seq(0, 4),
            Y <- lists:seq(0, 4),
            Level <- lists:seq(-Levels, Levels)
        ]),
    maps:fold(fun({X, Y}, Value, Acc) -> Acc#{{0, X, Y} => Value} end, Map0, Map).

neigbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}];
%? (middle)
neigbours({_, 2, 2}) ->
    [];
neigbours({Level, X, Y}) ->
    Base = [{Level, X, Y - 1}, {Level, X, Y + 1}, {Level, X - 1, Y}, {Level, X + 1, Y}],

    Extra =
        case {X, Y} of
            {0, 0} ->
                %a
                [8, 12];
            {0, 4} ->
                %u
                [12, 18];
            {4, 0} ->
                %e
                [8, 14];
            {4, 4} ->
                %y
                [14, 18];
            {0, _} ->
                % f,k,p
                [12];
            {4, _} ->
                % j,o,t
                [14];
            {_, 0} ->
                % b,c,d
                [8];
            {_, 4} ->
                % v,w,x
                [18];
            {2, 1} ->
                %h
                [a, b, c, d, e];
            {1, 2} ->
                %l
                [a, f, k, p, u];
            {3, 2} ->
                %n
                [e, j, o, t, y];
            {2, 3} ->
                %r
                [u, v, w, x, y];
            {_, _} ->
                % g,i,s,q
                []
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
