-module(aoc2021_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    step(Data, 1).

star2(_Data) ->
    {done, "Got 50 stars: Remotly start slead"}.

read(File) ->
    tools:read_grid(
        File,
        #{
            $> => east,
            $v => south,
            $. => empty
        }
    ).

step(Map, Step) ->
    East = maps:fold(east(Map), #{}, Map),
    MovedEast = maps:merge(Map, East),
    South = maps:fold(south(MovedEast), #{}, MovedEast),
    Result = maps:merge(MovedEast, South),

    Movements = maps:size(East) div 2 + maps:size(South) div 2,

    case Movements of
        0 ->
            Step;
        _ ->
            step(Result, Step + 1)
    end.

east(#{max := {MaxX, _}} = Grid) ->
    fun
        ({X, Y} = Pos, east, Acc) when X == MaxX ->
            case maps:get({0, Y}, Grid) of
                empty ->
                    Acc1 = maps:put(Pos, empty, Acc),
                    maps:put({0, Y}, east, Acc1);
                _ ->
                    Acc
            end;
        ({X, Y} = Pos, east, Acc) ->
            case maps:get({X + 1, Y}, Grid) of
                empty ->
                    Acc1 = maps:put(Pos, empty, Acc),
                    maps:put({X + 1, Y}, east, Acc1);
                _ ->
                    Acc
            end;
        (_, _, Acc) ->
            Acc
    end.

south(#{max := {_, MaxY}} = Grid) ->
    fun
        ({X, Y} = Pos, south, Acc) when Y == MaxY ->
            case maps:get({X, 0}, Grid) of
                empty ->
                    Acc1 = maps:put(Pos, empty, Acc),
                    maps:put({X, 0}, south, Acc1);
                _ ->
                    Acc
            end;
        ({X, Y} = Pos, south, Acc) ->
            case maps:get({X, Y + 1}, Grid) of
                empty ->
                    Acc1 = maps:put(Pos, empty, Acc),
                    maps:put({X, Y + 1}, south, Acc1);
                _ ->
                    Acc
            end;
        (_, _, Acc) ->
            Acc
    end.
