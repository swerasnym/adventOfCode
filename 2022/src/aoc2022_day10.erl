-module(aoc2022_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 10}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_lines(File).

star1(Data) ->
    {ValeLists, _} = lists:mapfoldl(fun runi/2, 1, Data),
    Values = [1] ++ lists:flatten(ValeLists),
    Strengths = [V * I || {I, V} <- lists:enumerate(Values), I rem 40 == 20, I =< 220],
    lists:sum(Strengths).

star2(Data) ->
    {ValeLists, _} = lists:mapfoldl(fun runi/2, 1, Data),
    Values = [1] ++ lists:flatten(ValeLists),
    Display = [draw(E) || E <- lists:enumerate(0, Values)],
    tools:print_grid(maps:from_list(Display)),
    manual.

runi("noop", X) ->
    {[X], X};
runi("addx " ++ N, X) ->
    [[V]] = tools:parse_format(N, "~d"),
    {[X, X + V], X + V}.

draw({I, V}) ->
    P = I rem 40,
    R = I div 40,

    Symbol =
        case V - P of
            -1 ->
                $#;
            0 ->
                $#;
            1 ->
                $#;
            _ ->
                $\s
        end,
    {{P, R}, Symbol}.
