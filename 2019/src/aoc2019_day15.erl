-module(aoc2019_day15).

-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
        star1 ->
            star1(Program);
        star2 ->
            star2(Program);
        _ ->
            Star1 = star1(Program),
            Star2 = star2(Program),
            {Star1, Star2}
    end.

star1(Program) ->
    Program1 = intcode:set_output_pid(self(), Program),

    Pid = intcode:spawn(Program1),

    {Steps, _Oxygen, _Maze1} = solve(Pid, {0, 0}, #{{0, 0} => {start, south}}, 0),
    intcode:send(Pid, halt),
    Steps.

star2(Program) ->
    Program1 = intcode:set_output_pid(self(), Program),
    Pid = intcode:spawn(Program1),

    {Steps, Oxygen, Maze1} = solve(Pid, {0, 0}, #{{0, 0} => {start, south}}, 0),
    Maze2 = solve(Pid, Oxygen, Maze1, Steps),

    intcode:send(Pid, halt),
    fill(Maze2#{Oxygen => 0}, [Oxygen], 0).

fill(Maze, [], Time) ->
    paint(Maze),
    Time;
fill(Maze, [Head | Rest], _) ->
    Time = maps:get(Head, Maze),

    Directions = directions(),
    Dirs =
        lists:filter(fun(Dir) ->
                        case maps:get(move(Head, Dir), Maze) of
                            {open, _} ->
                                true;
                            _ ->
                                false
                        end
                     end,
                     Directions),

    New = [move(Head, Dir) || Dir <- Dirs],

    Maze1 = lists:foldl(fun(Pos, Acc) -> Acc#{Pos => Time + 1} end, Maze, New),

    fill(Maze1, Rest ++ New, Time).

solve(Pid, Pos, Maze, Steps) ->
    {_, Back} = maps:get(Pos, Maze),
    Directions = directions(),

    New = lists:filter(fun(Dir) -> maps:get(move(Pos, Dir), Maze, new) == new end,
                       Directions),

    case New of
        [] ->
            case Pos of
                {0, 0} ->
                    paint(Maze),
                    Maze;
                %% error(dnoe);
                _ ->
                    intcode:send(Pid, direction(Back)),
                    {ok, [1]} = intcode:recvn(Pid, 1, 1000),
                    solve(Pid, move(Pos, Back), Maze, Steps - 1)
            end;
        [Head | _] ->
            intcode:send(Pid, direction(Head)),

            {ok, [Code]} = intcode:recvn(Pid, 1, 1000),
            Result = result(Code),
            Maze1 = Maze#{move(Pos, Head) => {Result, reverse(Head)}},
            case Result of
                wall ->
                    solve(Pid, Pos, Maze1, Steps);
                open ->
                    solve(Pid, move(Pos, Head), Maze1, Steps + 1);
                leak ->
                    paint(Maze1),
                    {Steps + 1, move(Pos, Head), Maze1}
            end
    end.

paint(Hull) ->
    Xs = [X || {X, _} <- maps:keys(Hull)],
    Ys = [Y || {_, Y} <- maps:keys(Hull)],

    [paint({X, Y}, Hull, lists:max(Xs))
     || Y
            <- lists:seq(
                   lists:min(Ys), lists:max(Ys)),
        X
            <- lists:seq(
                   lists:min(Xs), lists:max(Xs))],

    io:format("~n~n", []).

paint({X, _} = Pos, Hull, X) ->
    case maps:get(Pos, Hull, unknown) of
        {start, _} ->
            io:format(" S ~n", []);
        unknown ->
            io:format("???~n", []);
        {open, _} ->
            io:format(" . ~n", []);
        {wall, _} ->
            io:format("###~n", []);
        {leak, _} ->
            io:format(" X ~n", []);
        Number ->
            io:format(" ~B ~n", [Number rem 1000])
    end;
paint(Pos, Hull, _) ->
    case maps:get(Pos, Hull, unknown) of
        unknown ->
            io:format("???", []);
        {start, _} ->
            io:format(" S ", []);
        {open, _} ->
            io:format(" . ", []);
        {wall, _} ->
            io:format("###", []);
        {leak, _} ->
            io:format(" X ", []);
        Number ->
            io:format("~2.36B ", [Number rem (36 * 36)])
    end.

direction(north) ->
    [1];
direction(south) ->
    [2];
direction(east) ->
    [3];
direction(west) ->
    [4].

directions() ->
    [north, south, east, west].

move({X, Y}, north) ->
    {X, Y - 1};
move({X, Y}, south) ->
    {X, Y + 1};
move({X, Y}, east) ->
    {X + 1, Y};
move({X, Y}, west) ->
    {X - 1, Y}.

reverse(Dir) ->
    turn(turn(Dir)).

turn(north) ->
    west;
turn(west) ->
    south;
turn(south) ->
    east;
turn(east) ->
    north.

result(0) ->
    wall;
result(1) ->
    open;
result(2) ->
    leak.
