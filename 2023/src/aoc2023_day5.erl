-module(aoc2023_day5).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day5.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

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
     || [Ds, Rs, Range] <- tools:parse_lines(Numbers, parse_integers)
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
    case tools:intervals_overlapp(V, Range) of
        false ->
            map2(V, Rest);
        true ->
            remove_first_if_empty([
                tools:interval_before(V, Range),
                tools:interval_shift(tools:interval_inside(V, Range), Diff)
            ]) ++
                map2(tools:interval_after(V, Range), Rest)
    end.
