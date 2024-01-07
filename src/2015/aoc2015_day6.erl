-module(aoc2015_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Instructions) ->
    Grid = atomics:new(1000 * 1000, []),
    End = follow_instructions(Instructions, Grid),
    lists:sum([atomics:get(End, Pos) || Pos <- lists:seq(1, 1000_000)]).

star2(Instructions) ->
    Grid = atomics:new(1000 * 1000, []),
    End = follow_instructions2(Instructions, Grid),
    lists:sum([atomics:get(End, Pos) || Pos <- lists:seq(1, 1000_000)]).

read(File) ->
    tools:read_lines(File, fun parse_command/1).

parse_command("turn off " ++ Range) ->
    [Xmin, Ymin, Xmax, Ymax] = tools:parse_format(Range, "~d,~d through ~d,~d"),
    {off, {Xmin, Xmax}, {Ymin, Ymax}};
parse_command("turn on " ++ Range) ->
    [Xmin, Ymin, Xmax, Ymax] = tools:parse_format(Range, "~d,~d through ~d,~d"),
    {on, {Xmin, Xmax}, {Ymin, Ymax}};
parse_command("toggle " ++ Range) ->
    [Xmin, Ymin, Xmax, Ymax] = tools:parse_format(Range, "~d,~d through ~d,~d"),
    {toggle, {Xmin, Xmax}, {Ymin, Ymax}}.

follow_instructions([], Map) ->
    Map;
follow_instructions([{on, {Xmin, Xmax}, {Ymin, Ymax}} | Instructions], Map) ->
    [
        atomics:put(Map, X + 1000 * Y + 1, 1)
     || X <- lists:seq(Xmin, Xmax), Y <- lists:seq(Ymin, Ymax)
    ],
    follow_instructions(Instructions, Map);
follow_instructions([{off, {Xmin, Xmax}, {Ymin, Ymax}} | Instructions], Map) ->
    [
        atomics:put(Map, X + 1000 * Y + 1, 0)
     || X <- lists:seq(Xmin, Xmax), Y <- lists:seq(Ymin, Ymax)
    ],
    follow_instructions(Instructions, Map);
follow_instructions([{toggle, {Xmin, Xmax}, {Ymin, Ymax}} | Instructions], Map) ->
    [
        atomics:put(Map, X + 1000 * Y + 1, toggle(atomics:get(Map, X + 1000 * Y + 1)))
     || X <- lists:seq(Xmin, Xmax), Y <- lists:seq(Ymin, Ymax)
    ],
    follow_instructions(Instructions, Map).

follow_instructions2([], Map) ->
    Map;
follow_instructions2([{toggle, {Xmin, Xmax}, {Ymin, Ymax}} | Instructions], Map) ->
    [
        atomics:add(Map, X + 1000 * Y + 1, 2)
     || X <- lists:seq(Xmin, Xmax), Y <- lists:seq(Ymin, Ymax)
    ],
    follow_instructions2(Instructions, Map);
follow_instructions2([{on, {Xmin, Xmax}, {Ymin, Ymax}} | Instructions], Map) ->
    [
        atomics:add(Map, X + 1000 * Y + 1, 1)
     || X <- lists:seq(Xmin, Xmax), Y <- lists:seq(Ymin, Ymax)
    ],
    follow_instructions2(Instructions, Map);
follow_instructions2([{off, {Xmin, Xmax}, {Ymin, Ymax}} | Instructions], Map) ->
    [
        atomics:put(Map, X + 1000 * Y + 1, max(0, atomics:get(Map, X + 1000 * Y + 1) - 1))
     || X <- lists:seq(Xmin, Xmax), Y <- lists:seq(Ymin, Ymax)
    ],
    follow_instructions2(Instructions, Map).

toggle(0) -> 1;
toggle(1) -> 0.
