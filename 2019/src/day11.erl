-module(day11).

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
    Pid = intcode:spawn(Program, [{outputpid, self()}]),
    {ok, Hull} = robot(Pid),
    paint(Hull),
    maps:size(Hull).

star2(Program) ->
    Pid = intcode:spawn(Program, [{outputpid, self()}]),
    {ok, Hull} = robot(Pid, white),

    paint(Hull),
    "LPZKLGHR".

robot(Pid) ->
    robot(Pid, black).

robot(Pid, Color) ->
    robot(Pid, #{{0, 0} => Color}, {0, 0}, north).

robot(Pid, Hull, Pos, Dir) ->
    intcode:send(Pid, read_panel(Pos, Hull)),

    case intcode:recvn(Pid, 2, 1000) of
        {ok, [Color, Rotate]} ->
            Hull1 = write_panel(Pos, Hull, Color),
            Dir1 = turn(Dir, Rotate),
            Pos1 = move(Pos, Dir1),
            robot(Pid, Hull1, Pos1, Dir1);
        {halt, []} ->
            {ok, Hull}
    end.

turn(north, 0) ->
    west;
turn(north, 1) ->
    east;
turn(west, 0) ->
    south;
turn(west, 1) ->
    north;
turn(south, 0) ->
    east;
turn(south, 1) ->
    west;
turn(east, 0) ->
    north;
turn(east, 1) ->
    south.

move({X, Y}, north) ->
    {X, Y - 1};
move({X, Y}, south) ->
    {X, Y + 1};
move({X, Y}, east) ->
    {X + 1, Y};
move({X, Y}, west) ->
    {X - 1, Y}.

read_panel(Pos, Hull) ->
    case maps:get(Pos, Hull, black) of
        black ->
            [0];
        white ->
            [1]
    end.

write_panel(Pos, Hull, Color) ->
    Hull#{Pos => color(Color)}.

color(0) ->
    black;
color(1) ->
    white.

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
    ok.

paint({X, _} = Pos, Hull, X) ->
    case maps:get(Pos, Hull, black) of
        black ->
            io:format(" ~n", []);
        white ->
            io:format("#~n", [])
    end;
paint(Pos, Hull, _) ->
    case maps:get(Pos, Hull, black) of
        black ->
            io:format(" ", []);
        white ->
            io:format("#", [])
    end.
