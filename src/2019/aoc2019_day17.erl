-module(aoc2019_day17).
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
        problem => {2019, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    Pid = intcode:spawn(Program, [{outputpid, self()}, {inputpid, self()}]),
    {halt, View} = intcode:recvn(Pid, all),

    Map = scan(View),

    lists:sum([intersection(Pos, Map) || Pos <- maps:keys(Map)]).

star2(Program) ->
    Pid = intcode:spawn(Program, [{outputpid, self()}, {inputpid, self()}]),
    {halt, View} = intcode:recvn(Pid, all),
    Map = scan(View),
    path(Map),

    %% TODO Extract input programatically.
    Result =
        intcode:run(
            Program,
            [
                {set, 2, 0},
                {input,
                    "A,A,B,C,B,C,B,C,B,A\nR,10,L,12,R,6\nR,6,R,10,R,12,R,6\nR,10,L,12,L,1"
                    "2\nN\n"}
            ]
        ),
    Output = intcode:get_output(Result),
    lists:last(Output).

read(File) ->
    intcode:from_file(File).

scan(View) ->
    io:format("~s", [View]),
    scan(View, 0, 0, #{}).

scan([], _, _, Acc) ->
    Acc;
scan([$. | Rest], X, Y, Acc) ->
    scan(Rest, X + 1, Y, Acc);
scan([$\n | Rest], _, Y, Acc) ->
    scan(Rest, 0, Y + 1, Acc);
scan([First | Rest], X, Y, Acc) ->
    Term =
        case First of
            $# ->
                scaffold;
            $^ ->
                {robot, north};
            $v ->
                {robot, south};
            $> ->
                {robot, west};
            $< ->
                {robot, east}
        end,
    scan(Rest, X + 1, Y, Acc#{{X, Y} => Term}).

intersection(Pos, Map) ->
    case neigbours(Pos, Map) of
        {scaffold, scaffold, scaffold, scaffold} ->
            alignment(Pos);
        _ ->
            0
    end.

alignment({X, Y}) ->
    X * Y.

neigbours({X, Y}, Map) ->
    {
        maps:get({X, Y + 1}, Map, empty),
        maps:get({X, Y - 1}, Map, empty),
        maps:get({X + 1, Y}, Map, empty),
        maps:get({X - 1, Y}, Map, empty)
    }.

path(Map) ->
    [{Pos, {robot, Direction}}] =
        maps:to_list(
            maps:filter(
                fun(_, V) ->
                    case V of
                        {robot, _} ->
                            true;
                        _ ->
                            false
                    end
                end,
                Map
            )
        ),

    path(Pos, Direction, Map, []).

path(Pos, Direction, Map, Acc) ->
    case
        {
            maps:get(move(Pos, turn(Direction, $R)), Map, none),
            maps:get(move(Pos, turn(Direction, $L)), Map, none)
        }
    of
        {scaffold, _} ->
            NewDir = turn(Direction, $R),
            {Pos1, Distance} = find_end(Pos, NewDir, Map, 0),
            Command = lists:concat(["R", ",", Distance, ","]),

            path(Pos1, NewDir, Map, [Command | Acc]);
        {_, scaffold} ->
            NewDir = turn(Direction, $L),
            {Pos1, Distance} = find_end(Pos, NewDir, Map, 0),
            Command = lists:concat(["L", ",", Distance, ","]),

            path(Pos1, NewDir, Map, [Command | Acc]);
        _ ->
            lists:droplast(lists:flatten(lists:reverse(Acc)))
    end.

find_end(Pos0, Dir, Map, N) ->
    Pos = move(Pos0, Dir),
    case maps:get(Pos, Map, none) of
        scaffold ->
            find_end(Pos, Dir, Map, N + 1);
        _ ->
            {Pos0, N}
    end.

turn(north, $L) ->
    west;
turn(north, $R) ->
    east;
turn(west, $L) ->
    south;
turn(west, $R) ->
    north;
turn(south, $L) ->
    east;
turn(south, $R) ->
    west;
turn(east, $L) ->
    north;
turn(east, $R) ->
    south.

move({X, Y}, north) ->
    {X, Y - 1};
move({X, Y}, south) ->
    {X, Y + 1};
move({X, Y}, east) ->
    {X + 1, Y};
move({X, Y}, west) ->
    {X - 1, Y}.
