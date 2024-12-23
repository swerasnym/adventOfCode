-module(aoc2016_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day8_ex.txt", {star1, {6, 2}}, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Operations) ->
    star1(Operations, {49, 5}).

star1(Data, Max) ->
    Display = lists:foldl(fun apply/2, #{max => Max}, Data),
    maps:size(Display) - 1.

star2(Data) ->
    Display = lists:foldl(fun apply/2, #{max => {49, 5}}, Data),
    tools:print_grid(Display),
    aoc_ocr:decode(Display, $█).

read(File) ->
    tools:read_lines(File, fun parse/1).

parse("rect " ++ Rest) ->
    [Cols, Rows] = tools:parse_format(Rest, "~dx~d"),
    {rect, {Cols, Rows}};
parse("rotate column x=" ++ Rest) ->
    [Col, Steps] = tools:parse_format(Rest, "~d by ~d"),
    {rot, {x, Col, Steps}};
parse("rotate row y=" ++ Rest) ->
    [Row, Steps] = tools:parse_format(Rest, "~d by ~d"),
    {rot, {y, Row, Steps}}.

apply({rot, {x, Col, Steps}}, Screen = #{max := {_, Ymax}}) ->
    Pos = [{Col, Y} || {C, Y} := _ <- Screen, C == Col],
    Remove = maps:without(Pos, Screen),
    maps:merge(Remove, #{{X, tools:mod(Y + Steps, Ymax + 1)} => $█ || {X, Y} <- Pos});
apply({rot, {y, Row, Steps}}, Screen = #{max := {Xmax, _}}) ->
    Pos = [{X, Row} || {X, R} := _ <- Screen, R == Row],
    Remove = maps:without(Pos, Screen),
    maps:merge(Remove, #{{tools:mod(X + Steps, Xmax + 1), Y} => $█ || {X, Y} <- Pos});
apply({rect, {Cols, Rows}}, Screen) ->
    maps:merge(Screen, #{{X, Y} => $█ || X <- lists:seq(0, Cols - 1), Y <- lists:seq(0, Rows - 1)}).
