-module(aoc2021_day25).

-export([run/2, profile/3, eprof/2]).

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

profile(Star, File, Times) ->
    Data = read(File),
    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [
            begin
                {Time, Expected} = timer:tc(F),
                Time
            end
         || _ <- lists:seq(1, Times)
        ],
    {Expected, lists:sum(Results) / Times / 1000}.

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1(Data) ->
    step(Data, 1).

star2(_Data) ->
    "Got 50 stars: Remotly start slead".

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
