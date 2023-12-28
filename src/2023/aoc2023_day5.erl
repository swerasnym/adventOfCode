-module(aoc2023_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day5_ex.txt", star1, 35},
        {"examples/2023/day5_ex.txt", star2, 46}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1({Seeds, Maps}) ->
    lists:min([map_all(S, Maps) || S <- Seeds]).

star2({Seeds, Maps}) ->
    Intervalls = [tools:interval_from_length(S, C) || {S, C} <- tools:group(2, Seeds)],
    MapAll = map_all2(Intervalls, Maps),
    {Res, _} = lists:min(MapAll),
    Res.

read(File) ->
    [Seeds | MapBlocks] = tools:read_blocks(File),
    {parse_seeds(Seeds), [parse_maps(Map) || Map <- MapBlocks]}.

parse_seeds("seeds: " ++ Seeds) ->
    tools:parse_integers(Seeds).

parse_maps(Map) ->
    [_Text, Numbers] = string:split(Map, "\n"),
    Maps = [
        {tools:interval_from_length(Rs, Range), Ds - Rs}
     || [Ds, Rs, Range] <- tools:parse_lines(Numbers, fun tools:parse_integers/1)
    ],

    lists:sort(Maps).

map_all(R, []) ->
    R;
map_all(S, [Map | Rest]) ->
    map_all(map(S, Map), Rest).

map(V, []) ->
    V;
map(V, [{{Rs, Re}, Diff} | _]) when V >= Rs, V < Re ->
    V + Diff;
map(V, [_ | Rest]) ->
    map(V, Rest).

map_all2(Vs, []) ->
    Vs;
map_all2(Vs, [Map | Rest]) ->
    New = [map2(V, Map) || V <- Vs],
    map_all2(lists:flatten(New), Rest).

remove_first_if_empty([empty | Rest]) ->
    Rest;
remove_first_if_empty(All) ->
    All.

map2(empty, _) ->
    [];
map2(V, []) ->
    [V];
map2(V, [{Range, Diff} | Rest]) ->
    case tools:intervals_overlap(V, Range) of
        false ->
            map2(V, Rest);
        true ->
            remove_first_if_empty([
                tools:interval_before(V, Range),
                tools:interval_shift(tools:interval_inside(V, Range), Diff)
            ]) ++
                map2(tools:interval_after(V, Range), Rest)
    end.
