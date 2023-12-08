-module(aoc2023_day8).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day8.txt"),
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

star1({Path, Map}) ->
    steps("AAA", Path, "ZZZ", Map).

star2({Path, Map}) ->
    As = lists:filter(fun ends_in_a/1, maps:keys(Map)),
    Zs = lists:filter(fun ends_in_z/1, maps:keys(Map)),
    %% My data had exactly one reachable goal for each start, so I tried to just LCM them together It worked!.
    tools:lcm([S || A <- As, Z <- Zs, (S = steps(A, Path, Z, Map)) /= unreachable]).

read(File) ->
    [[Path], Dirs0] = tools:read_blocks(File, parse_lines),

    Dirs1 = [string:tokens(D, " =,()") || D <- Dirs0],

    {Path, #{Dir => {L, R} || [Dir, L, R] <- Dirs1}}.

steps(Start, Path, Goal, Map) ->
    steps(Start, Path, Path, Goal, Map, 0, #{}).

steps(Pos, [], Path0, Goal, Map, Count, Visited) ->
    steps(Pos, Path0, Path0, Goal, Map, Count, Visited);
steps(Goal, _, _, Goal, _, Count, _) ->
    Count;
steps(Pos, Path, _, _, _, _, Visited) when is_map_key({Pos, length(Path)}, Visited) ->
    unreachable;
steps(Pos, [Dir | Rest] = Path, Path0, Goal, Map, Count, Visited) ->
    {L, R} = maps:get(Pos, Map),
    case Dir of
        $L ->
            steps(L, Rest, Path0, Goal, Map, Count + 1, Visited#{{Pos, length(Path)} => true});
        $R ->
            steps(R, Rest, Path0, Goal, Map, Count + 1, Visited#{{Pos, length(Path)} => true})
    end.

ends_in_a([_, _, Letter]) ->
    $A == Letter.

ends_in_z([_, _, Letter]) ->
    $Z == Letter.
