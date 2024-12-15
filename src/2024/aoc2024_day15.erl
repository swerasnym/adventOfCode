-module(aoc2024_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day15_ex.txt", star1, 10092},
        {"examples/2024/day15_ex2.txt", star1, 2028},
        {"examples/2024/day15_ex.txt", star2, 9021},
        {"examples/2024/day15_ex3.txt", star2, 618}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

-define(BL, $[).
-define(BR, $]).

star1({Directions, Maze}) ->
    [Start] = [P || P := $@ <- Maze],
    {_Pos, MazeOut} = simulate(Directions, Start, Maze),
    tools:print_grid(MazeOut),
    lists:sum([gps(Pos) || Pos := $O <- MazeOut]).

star2({Directions, MazeIn}) ->
    Maze = expand(MazeIn),
    [Start] = [P || P := $@ <- Maze],
    {_Pos, MazeOut} = simulate(Directions, Start, Maze),
    tools:print_grid(MazeOut),
    lists:sum([gps(Pos) || Pos := ?BL <- MazeOut]).

read(File) ->
    [MazeStr, DirectionsStr] = tools:read_blocks(File),
    Directions = lists:flatten(string:replace(DirectionsStr, "\n", "", all)),
    Maze = tools:parse_grid(MazeStr),
    {Directions, Maze}.

simulate([], Pos, Maze) ->
    {Pos, Maze};
simulate([Dir | Rest], Pos, Maze) ->
    {Pos1, Maze1} = update(Dir, Pos, Maze),
    simulate(Rest, Pos1, Maze1).

update(Dir, Pos, Maze) ->
    Next = move(Dir, Pos),
    Symb = maps:get(Pos, Maze),
    case maps:get(Next, Maze) of
        $. ->
            {Next, Maze#{Pos => $., Next => Symb}};
        $# ->
            {Pos, Maze};
        $O ->
            case update(Dir, Next, Maze) of
                {Next, Maze} ->
                    {Pos, Maze};
                {_, Maze1} ->
                    {Next, Maze1#{Pos => $., Next => Symb}}
            end;
        % Star 2 updates below!
        _ when Dir == $< orelse Dir == $> ->
            case update(Dir, Next, Maze) of
                {Next, Maze} ->
                    {Pos, Maze};
                {_, Maze1} ->
                    {Next, Maze1#{Pos => $., Next => Symb}}
            end;
        ?BL ->
            NextL = Next,
            NextR = move(Dir, move($>, Pos)),
            {NewL, MazeL} = update(Dir, NextL, Maze),
            {NewR, MazeR} = update(Dir, NextR, MazeL),
            case {NewL, NewR} of
                {NextL, _} ->
                    {Pos, Maze};
                {_, NextR} ->
                    {Pos, Maze};
                _ ->
                    {Next, MazeR#{Pos => $., Next => Symb}}
            end;
        ?BR ->
            NextL = move(Dir, move($<, Pos)),
            NextR = Next,
            {NewL, MazeL} = update(Dir, NextL, Maze),
            {NewR, MazeR} = update(Dir, NextR, MazeL),
            case {NewL, NewR} of
                {NextL, _} ->
                    {Pos, Maze};
                {_, NextR} ->
                    {Pos, Maze};
                _ ->
                    {Next, MazeR#{Pos => $., Next => Symb}}
            end
    end.

move($^, {X, Y}) -> {X, Y - 1};
move($v, {X, Y}) -> {X, Y + 1};
move($>, {X, Y}) -> {X + 1, Y};
move($<, {X, Y}) -> {X - 1, Y}.

gps({X, Y}) -> X + 100 * Y.

expand(Maze) ->
    Left = #{{X * 2, Y} => left(Symb) || {X, Y} := Symb <- Maze},
    Right = #{{X * 2 + 1, Y} => right(Symb) || {X, Y} := Symb <- Maze},
    maps:merge(Left, Right).

left($O) ->
    ?BL;
left(Other) ->
    Other.

right($O) ->
    ?BR;
right($#) ->
    $#;
right(_) ->
    $..
