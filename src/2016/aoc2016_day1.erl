-module(aoc2016_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day1_ex.txt", star1, 12},
        {"examples/2016/day1_ex2.txt", star2, 4}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Directions) ->
    {X, Y} = navigate({0, 0}, north, Directions),
    abs(X) + abs(Y).

star2(Directions) ->
    {X, Y} = navigate2({0, 0}, north, Directions, #{}),
    abs(X) + abs(Y).

read(File) ->
    [parse_turn(T) || T <- string:split(tools:read_string(File), ", ", all)].

navigate(Pos, _, []) ->
    Pos;
navigate(Pos, Heading, [{Turn, Dist} | Rest]) ->
    NewHeading = turn(Heading, Turn),
    navigate(move(Pos, Dist, NewHeading), NewHeading, Rest).

navigate2(Pos, _Heading, _, Map) when is_map_key(Pos, Map) ->
    Pos;
navigate2(Pos, Heading, [{_, 0} | Rest], Map) ->
    navigate2(Pos, Heading, Rest, Map);
navigate2(Pos, Heading, [{Turn, Dist} | Rest], Map) ->
    NewHeading = turn(Heading, Turn),
    navigate2(move(Pos, 1, NewHeading), NewHeading, [{none, Dist - 1} | Rest], Map#{Pos => true}).

parse_turn("R" ++ Dist) ->
    {right, list_to_integer(Dist)};
parse_turn("L" ++ Dist) ->
    {left, list_to_integer(Dist)}.

turn(north, left) ->
    west;
turn(north, right) ->
    east;
turn(west, left) ->
    south;
turn(west, right) ->
    north;
turn(south, left) ->
    east;
turn(south, right) ->
    west;
turn(east, left) ->
    north;
turn(east, right) ->
    south;
turn(Dir, none) ->
    Dir.

move(Pos, Dist, north) ->
    aoc_vector:add(Pos, {0, -Dist});
move(Pos, Dist, south) ->
    aoc_vector:add(Pos, {0, Dist});
move(Pos, Dist, east) ->
    aoc_vector:add(Pos, {Dist, 0});
move(Pos, Dist, west) ->
    aoc_vector:add(Pos, {-Dist, 0}).
