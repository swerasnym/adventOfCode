-module(aoc2022_day24).
-behaviour(aoc_solution).
-hank([{unnecessary_function_arguments, [reached_start]}]).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 24}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:replace(
        tools:read_grid(File),
        #{
            $# => wall,
            $. => [],
            $> => [east],
            $< => [west],
            $^ => [north],
            $v => [south]
        }
    ).

star1(Map) ->
    {N, End} = bfs(0, [{1, 0}], update_map(Map), fun reached_goal/2),
    print(End),
    N.

star2(#{max := {Xmax, Ymax}} = Map) ->
    {T1, Map1} = bfs(0, [{1, 0}], update_map(Map), fun reached_goal/2),
    {T2, Map2} = bfs(1, [{Xmax - 1, Ymax}], update_map(Map1), fun reached_start/2),
    {T3, End} = bfs(1, [{1, 0}], update_map(Map2), fun reached_goal/2),
    print(End),
    io:format("~p~n", [[T1, T2, T3]]),
    T1 + T2 + T3.

bfs(N, Ps, Map, Goal) when length(Ps) > 0 ->
    Next = lists:uniq(lists:flatten([moves(P, Map) || P <- Ps])),
    % io:format("l ~p~n", [Next]),
    case Goal(Ps, Map) of
        true ->
            {N, Map};
        false ->
            bfs(N + 1, Next, update_map(Map), Goal)
    end.

moves(P, #{max := {Xmax, Ymax}} = Map) ->
    %% io:format("~p", [P]),
    [
        {X, Y}
     || {X, Y} <- [P, move(P, north), move(P, south), move(P, east), move(P, west)],
        Y >= 0,
        Y =< Ymax,
        X >= 0,
        X =< Xmax,
        maps:get({X, Y}, Map, []) == []
    ].

reached_goal(Ps, #{max := {_, Ymax}}) ->
    length([P || P = {_, Y} <- Ps, Y == Ymax]) > 0.

reached_start(Ps, _) ->
    length([P || P = {_, Y} <- Ps, Y == 0]) > 0.

update_map(Map) ->
    NewList = lists:flatten([update(KV, Map) || KV <- maps:to_list(Map)]),
    NewWrappedMax = tools:group_kv(NewList),
    [Max] = maps:get(max, NewWrappedMax),
    NewWrappedMax#{max => Max}.

update({P, wall}, _) ->
    {P, wall};
update({P, Dirs}, Map) when is_list(Dirs) ->
    [looped(move(P, Dir), Map, Dir) || Dir <- Dirs];
update(E, _) ->
    E.

print(Map) ->
    io:format("~n"),
    tools:print_grid(tools:replace(Map, fun replace/1)).

replace(wall) ->
    $#;
replace([wall]) ->
    $#;
replace([]) ->
    $.;
replace([east]) ->
    $>;
replace([west]) ->
    $<;
replace([north]) ->
    $^;
replace([south]) ->
    $v;
replace(List) when is_list(List) ->
    length(List) + $0;
replace(Other) ->
    Other.

move(P, wall) ->
    P;
move({X, Y}, north) ->
    {X, Y - 1};
move({X, Y}, south) ->
    {X, Y + 1};
move({X, Y}, east) ->
    {X + 1, Y};
move({X, Y}, west) ->
    {X - 1, Y}.

looped(P, _, wall) ->
    {P, wall};
looped({Xmax, Y}, #{max := {Xmax, _Ymax}}, Dir) ->
    {{1, Y}, Dir};
looped({0, Y}, #{max := {Xmax, _Ymax}}, Dir) ->
    {{Xmax - 1, Y}, Dir};
looped({X, Ymax}, #{max := {_Xmax, Ymax}}, Dir) ->
    {{X, 1}, Dir};
looped({X, 0}, #{max := {_Xmax, Ymax}}, Dir) ->
    {{X, Ymax - 1}, Dir};
looped(P, _, Dir) ->
    {P, Dir}.
