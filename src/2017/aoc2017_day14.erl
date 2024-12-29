-module(aoc2017_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    %% cSpell:ignore flqrgnkx
    Examples = [
        {{data, "flqrgnkx"}, star1, 8108},
        {{data, "flqrgnkx"}, star2, 1242}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 14},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(String) ->
    Map = build_map(String),
    tools:print_grid(tools:sub_grid(Map, {0, 0}, {7, 7})),
    tools:count($#, Map).

star2(String) ->
    Map = build_map(String),
    Starts = [Pos || Pos := $# <- Map],
    count_groups(Starts, neighbours(Map), 0).

read(File) ->
    tools:read_string(File).

build_map(String) ->
    Kv = [
        {{X, Y}, symbol(V)}
     || Y <- lists:seq(0, 127), {X, V} <- lists:enumerate(0, binary_hash(String, Y))
    ],
    maps:from_list(Kv).

binary_hash(String, Row) ->
    ToHash = String ++ "-" ++ integer_to_list(Row),
    tools:binary_to_bit_string(aoc2017_day10:knot_hash(ToHash)).

symbol($0) -> $.;
symbol($1) -> $#.

neighbours(Map) ->
    fun(Pos) ->
        Ns = [aoc_vector:add(Pos, Dp) || Dp <- [{1, 0}, {0, 1}, {0, -1}, {-1, 0}]],
        [N || N <- Ns, maps:get(N, Map, $.) == $#]
    end.

count_groups([], _, Count) ->
    Count;
count_groups([Hd | Rest], Neighbours, Count) ->
    {no_path, Reachable} = aoc_graph:bfs(Hd, none, Neighbours),
    count_groups(Rest -- maps:keys(Reachable), Neighbours, Count + 1).
