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
    combind([steps(A, Path, fun ends_in_z/1, Map) || A <- As]).

read(File) ->
    [[Path], Dirs0] = tools:read_blocks(File, parse_lines),
    Dirs1 = [string:tokens(D, " =,()") || D <- Dirs0],
    {Path, #{Dir => {L, R} || [Dir, L, R] <- Dirs1}}.

combind(Cycles) ->
    case tools:chinese_remainder([{C, M} || {[C], M} <- Cycles]) of
        {0, M} -> M;
        {X, _} -> X
    end.

steps(Start, Path, Goal, Map) ->
    steps(Start, Path, Path, Goal, Map, 0, #{}).

steps(Pos, [], Path0, Goal, Map, Count, Visited) ->
    steps(Pos, Path0, Path0, Goal, Map, Count, Visited);
steps(Goal, _, _, Goal, _, Count, _) ->
    Count;
steps(Pos, Path, _, _, _, Count, Visited) when is_map_key({Pos, length(Path)}, Visited) ->
    case maps:get(goals, Visited, unreachable) of
        unreachable ->
            unreachable;
        Goals ->
            Cycle = Count - maps:get({Pos, length(Path)}, Visited),
            {[G rem Cycle || G <- Goals], Cycle}
    end;
steps(Pos, [Dir | Rest] = Path, Path0, Goal, Map, Count, Visited) ->
    Next = select(Dir, maps:get(Pos, Map)),
    NextV = update_visited(Pos, Path, Count, Goal, Visited),
    steps(Next, Rest, Path0, Goal, Map, Count + 1, NextV).

update_visited(Pos, Path, Count, Goal, V) when is_function(Goal, 1) ->
    case Goal(Pos) of
        true ->
            V#{{Pos, length(Path)} => Count, goals => [Count | maps:get(goals, V, [])]};
        false ->
            V#{{Pos, length(Path)} => Count}
    end;
update_visited(Pos, Path, Count, _Goal, V) ->
    V#{{Pos, length(Path)} => Count}.

select($L, {L, _}) ->
    L;
select($R, {_, R}) ->
    R.
ends_in_a([_, _, Letter]) ->
    $A == Letter.

ends_in_z([_, _, Letter]) ->
    $Z == Letter.
