-module(aoc2017_day21).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day21_ex.txt", {star1, 2}, 12}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Rules) ->
    star1(Rules, 5).

star1(Rules, Steps) ->
    count_filled(Steps, Rules).

star2(Rules) ->
    count_filled(18, Rules).

read(File) ->
    Rules0 = tools:read_lines(File, fun parse_example/1),
    maps:from_list(lists:flatten([variations1(R) || R <- Rules0])).

start() ->
    tools:parse_grid(
        """
        .#.
        ..#
        ###
        """
    ).

parse_example(Line) ->
    [Ex, Out] = string:split(Line, " => "),
    {
        tools:lists_to_grid(string:split(Ex, "/", all)),
        tools:lists_to_grid(string:split(Out, "/", all))
    }.

variations1({Ex0, Out}) ->
    Ex1 = tools:rotate_grid(Ex0),
    Ex2 = tools:rotate_grid(Ex1),
    Ex3 = tools:rotate_grid(Ex2),

    Ex4 = tools:flip_grid(Ex0),
    Ex5 = tools:rotate_grid(Ex4),
    Ex6 = tools:rotate_grid(Ex5),
    Ex7 = tools:rotate_grid(Ex6),

    [
        {Ex0, Out},
        {Ex1, Out},
        {Ex2, Out},
        {Ex3, Out},
        {Ex4, Out},
        {Ex5, Out},
        {Ex6, Out},
        {Ex7, Out}
    ].

step(Rules) -> fun(Grid) -> step(Grid, Rules) end.

step(#{max := {Mx, _}} = Grid, Rules) when (Mx + 1) rem 2 == 0 ->
    step(2, (Mx + 1) div 2, Grid, Rules);
step(#{max := {Mx, _}} = Grid, Rules) when (Mx + 1) rem 3 == 0 ->
    step(3, (Mx + 1) div 3, Grid, Rules).

step(Size, N, Grid, Rules) ->
    MinMax = [
        {{X, Y}, bound(X, Y, Size)}
     || X <- lists:seq(0, N - 1), Y <- lists:seq(0, N - 1)
    ],
    Splits = [{C, tools:sub_grid(Grid, Min, Max)} || {C, {Min, Max}} <- MinMax],
    Updated = [{C, maps:get(G, Rules)} || {C, G} <- Splits],
    Translated = [tools:translate_grid(G, translate(C, Size + 1)) || {C, G} <- Updated],
    Output = lists:foldl(fun maps:merge/2, #{}, Translated),
    Output#{max => {(Size + 1) * N - 1, (Size + 1) * N - 1}}.

bound(X, Y, Size) ->
    {{X * Size, Y * Size}, {(X + 1) * Size - 1, (Y + 1) * Size - 1}}.
translate({X, Y}, Size) -> {X * Size, Y * Size}.

count_filled(Steps, Rules) ->
    {Res, _} = count_filled(Steps, start(), #{}, step(Rules)),
    Res.

count_filled(0, Grid, Mem, _) ->
    {tools:count($#, Grid), Mem};
count_filled(N, #{max := {2, 2}} = Grid, Mem, Step) when N >= 3 ->
    case maps:is_key({N, Grid}, Mem) of
        true ->
            {maps:get({N, Grid}, Mem), Mem};
        false ->
            Large = Step(Step(Step(Grid))),
            {8, 8} = maps:get(max, Large),
            MinMax = [
                {{X, Y}, bound(X, Y, 3)}
             || X <- lists:seq(0, 2), Y <- lists:seq(0, 2)
            ],
            SubGrids = [tools:sub_grid(Large, Min, Max) || {_, {Min, Max}} <- MinMax],
            Recurse = fun(G, {Count, Map}) ->
                {CountG, MapG} = count_filled(N - 3, G, Map, Step),
                {CountG + Count, MapG}
            end,
            {Res, MapOut} = lists:foldl(Recurse, {0, Mem}, SubGrids),
            {Res, MapOut#{{N, Grid} => Res}}
    end;
count_filled(N, Grid, Mem, Step) ->
    count_filled(N - 1, Step(Grid), Mem, Step).
