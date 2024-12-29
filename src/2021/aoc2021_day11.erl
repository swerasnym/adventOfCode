-module(aoc2021_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 11},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {C, Map} = step(Data, 100, 0),
    tools:print_grid(maps:map(fun(_, V) -> V + $0 end, Map)),

    C.

star2(Data) ->
    step2(Data, 1).

read(File) ->
    tools:drop_max(tools:read_grid(File, fun(V) -> V - $0 end)).

neighbours({X, Y} = Pos) ->
    lists:delete(Pos, [{X + Dx, Y + Dy} || Dx <- lists:seq(-1, 1), Dy <- lists:seq(-1, 1)]).

step(Map, 0, Count) ->
    {Count, Map};
step(Map, Steps, Count) ->
    Map1 = maps:map(fun(_K, V) -> V + 1 end, Map),
    {Map2, Fk} = iterate(Map1, []),
    Map3 =
        maps:map(
            fun
                (_K, V) when V > 9 ->
                    0;
                (_K, V) ->
                    V
            end,
            Map2
        ),
    step(Map3, Steps - 1, Count + length(Fk)).

step2(Map, Step) ->
    Map1 = maps:map(fun(_K, V) -> V + 1 end, Map),
    {Map2, Fk} = iterate(Map1, []),
    Map3 =
        maps:map(
            fun
                (_K, V) when V > 9 ->
                    0;
                (_K, V) ->
                    V
            end,
            Map2
        ),

    case maps:size(Map) == length(Fk) of
        true ->
            Step;
        false ->
            step2(Map3, Step + 1)
    end.

iterate(Map, Flashed) ->
    F = maps:filtermap(fun(_K, V) -> V > 9 end, maps:without(Flashed, Map)),
    case maps:keys(F) of
        [] ->
            {Map, Flashed};
        Fk ->
            iterate(inc(Map, Fk), Flashed ++ Fk)
    end.

inc(Map, Fk) ->
    Add = lists:flatmap(fun neighbours/1, Fk),
    lists:foldl(
        fun
            (K, AccIn) when is_map_key(K, AccIn) ->
                maps:update_with(K, fun(V) -> V + 1 end, AccIn);
            (_, AccIn) ->
                AccIn
        end,
        Map,
        Add
    ).
