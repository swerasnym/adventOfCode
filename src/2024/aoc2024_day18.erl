-module(aoc2024_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2024/day18_ex.txt", {star1, {{6, 6}, 12}}, 22},
        {"examples/2024/day18_ex.txt", {star2, {{6, 6}, 12}}, "6,1"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    star1(Data, {{70, 70}, 1024}).

star1(Data, {Max = {Xmax, Ymax}, ToAdd}) ->
    Map0 = #{{X, Y} => $. || X <- lists:seq(0, Xmax), Y <- lists:seq(0, Ymax)},
    Map1 = #{P => $# || P <- lists:sublist(Data, ToAdd)},
    Map = maps:merge(Map0, Map1),

    tools:print_grid(Map),
    {Dist, _, _} = aoc_graph:a_star([{0, 0}], is_end(Max), neighbours(Map), estimate(Max)),
    Dist.
star2(Data) ->
    star2(Data, {{70, 70}, 1024}).
star2(Data, {Max = {Xmax, Ymax}, ToAdd}) ->
    Map0 = #{{X, Y} => $. || X <- lists:seq(0, Xmax), Y <- lists:seq(0, Ymax)},
    {X, Y} = find_last2({ToAdd, length(Data)}, Data, Map0, Max),
    string:join([integer_to_list(I) || I <- [X, Y]], ",").

read(File) ->
    tools:group(2, tools:read_integers(File, "\n,")).

neighbours(Map) ->
    fun(Pos) ->
        Dn = [aoc_vector:add(Pos, Dp) || Dp <- [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]],
        [{1, D} || D <- Dn, maps:get(D, Map, $#) /= $#]
    end.

estimate({Xmax, Ymax}) ->
    fun({X, Y}) ->
        Xmax - X + Ymax - Y
    end.

is_end(Max) ->
    fun(Pos) ->
        Pos == Max
    end.

find_last2({N, N}, Data, _Map0, _Max) ->
    lists:nth(N, Data);
find_last2({NMin, NMax}, Data, Map0, Max) ->
    N = (NMin + NMax) div 2,
    Map1 = #{P => $# || P <- lists:sublist(Data, N)},
    Map = maps:merge(Map0, Map1),
    case aoc_graph:a_star([{0, 0}], is_end(Max), neighbours(Map), estimate(Max)) of
        {no_path, _} -> find_last2({NMin, N}, Data, Map0, Max);
        _ -> find_last2({N + 1, NMax}, Data, Map0, Max)
    end.
