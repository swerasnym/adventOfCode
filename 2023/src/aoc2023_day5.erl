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
    MapAll = map_all2(pair(Seeds, []), Maps),

    {Res, _} =  lists:min(MapAll),
    Res.

read(File) ->
    [Seeds| MapBlocks] = tools:read_blocks(File),
    {parse_seeds(Seeds), [parse_maps( Map)|| Map <- MapBlocks]} .

 parse_seeds("seeds: " ++ Seeds) ->
    tools:parse_integers(Seeds).


cmp({_, Rs1, _}, {_, Rs2, _}) ->
    Rs1 =< Rs2.

parse_maps(Map) ->
    [_Text, Numbers] = string:split(Map, "\n"),
    lists:sort(fun cmp/2, [erlang:list_to_tuple(L) || L <- tools:parse_lines(Numbers, parse_integers)]).




map(V, []) ->
            V;
map(V, [{Ds, Rs, Range}| Rest]) ->
    case V - Rs of
        N when 0 =< N, Range > N ->
            Ds + N;

        _ ->
            map(V, Rest)
        end.


map_all2(Vs, []) ->
    Vs;
map_all2(Vs, [Map| Rest]) ->
    New = [map2(V, Map) || V <- Vs],
    map_all2(lists:flatten(New), Rest).

map2(V, []) ->
        [V];
map2({Begin, Length} = V, [{Ds, Rs, Range}| Rest]) ->
    End = Begin + Length - 1,
    Rend = Rs + Range -1,
case {Begin =< Rend andalso Rs =< End, Begin < Rs, End > Rend} of
    {false, _, _} ->
        map2(V, Rest);
    {true, true, true} ->
        [{Begin, Rs-Begin}, {Ds, Range} | map2({Rs + Range, End - Rend}, Rest) ];

    {true, true, false}    ->
         [{Begin, Rs-Begin}, {Ds, End - Rs}];

    {true, false, true}  ->
         [{Ds+ Begin - Rs, Rend - Begin} | map2({Rs + Range, End - Rend}, Rest) ];
    {true, false, false} ->
         [{Ds+ Begin - Rs, Length}]
    end.



map_all(R, []) ->
            R;
map_all(S, [Map|Rest]) ->
    map_all(map(S, Map), Rest).

pair([], Res) ->
    lists:reverse(Res);
pair([A, B| Rest], Res) ->
    pair(Rest, [{A, B}| Res]).