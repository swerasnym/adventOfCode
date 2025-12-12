-module(aoc2025_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day12_ex.txt", star1, 0}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({_Packets, Areas}) ->
    {Trivial, _NonTrivial} = lists:partition(fun trivial/1, Areas),
    length(Trivial).

star2(_) ->
    {done, "Done Decorating"}.

read(File) ->
    [Packets, Regions] = string:split(tools:read_string(File), "\n\n", trailing),

    {parse_packets(Packets), tools:parse_lines(Regions, fun parse_region/1)}.

parse_packets(Packets) ->
    maps:from_list(tools:parse_blocks(Packets, fun parse_packet/1)).

parse_packet(Packet) ->
    [DigitS, GridS] = string:split(Packet, ":\n"),
    Grid = tools:parse_grid(GridS),
    NoEmpty = maps:filter(fun(K, V) -> V == $# orelse K == max end, Grid),

    {list_to_integer(DigitS), all_shapes(NoEmpty)}.

parse_region(Region) ->
    [Shape, NoPacketsS] = string:split(Region, ": "),
    [X, Y] = tools:parse_integers(Shape, "x"),
    NoPackets = tools:parse_integers(NoPacketsS, " "),
    {{X, Y}, lists:sum(NoPackets), lists:enumerate(0, NoPackets)}.

all_shapes(Packet) ->
    Flip = tools:flip_grid(Packet),
    lists:usort([
        Packet,
        tools:rotate_grid(Packet, cw),
        tools:rotate_grid(Packet, ccw),
        tools:rotate_grid(tools:rotate_grid(Packet)),
        Flip,
        tools:rotate_grid(Flip, cw),
        tools:rotate_grid(Flip, ccw),
        tools:rotate_grid(tools:rotate_grid(Flip))
    ]).

trivial({{W, H}, S, _}) ->
    (W div 3) * (H div 3) >= S.
