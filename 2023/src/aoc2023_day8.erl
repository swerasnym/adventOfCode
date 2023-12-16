-module(aoc2023_day8).
-behaviour(aoc_solution).

-export([run/0, run/2, star3/1]).

%% Comunity Bonus1 should give {276, 676}
%%  Bonus2 should give  {unreachable,19}
%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 8}, all => [star1, star2, star3]}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Path, Map}) ->
    steps("AAA", Path, "ZZZ", Map).

star2({Path, Map}) ->
    As = lists:filter(fun ends_in_a/1, maps:keys(Map)),
    Steps = [steps(A, Path, fun ends_in_z/1, Map) || A <- As],
    case [G || {{[G], G}, []} <- Steps] of
        Cycles when length(Cycles) == length(As) ->
            tools:lcm(Cycles);
        _ ->
            star3({Path, Map})
    end.

%% In the aoc data we always had perfect cycles allowing for just using LCM, star3 handles some more extreme inputs
star3({Path, Map}) ->
    As = lists:filter(fun ends_in_a/1, maps:keys(Map)),
    combind([steps(A, Path, fun ends_in_z/1, Map) || A <- As]).

read(File) ->
    [[Path], Dirs0] = tools:read_blocks(File, parse_lines),
    Dirs1 = [string:tokens(D, " =,()") || D <- Dirs0],
    {Path, #{Dir => {L, R} || [Dir, L, R] <- Dirs1}}.

combind(Paths) ->
    io:format("~p", [Paths]),
    %% Add a -1 to take care of the case when all values are inside all cycles...
    LatsVisitOutsideCycle = lists:max(lists:flatten([-1] ++ [L || {_, L} <- Paths])),
    EarlyVisits = [
        lists:usort(
            lists:flatten(
                L ++ [lists:seq(R, LatsVisitOutsideCycle, M) || R <- Rs, R < LatsVisitOutsideCycle]
            )
        )
     || {{Rs, M}, L} <- Paths
    ],

    case tools:overlap(EarlyVisits) of
        [] ->
            {WithCycles, WithoutCycles} = lists:partition(fun({{Rs, _}, _}) -> Rs /= [] end, Paths),
            CycleOnly = tools:chinese_multi_reminder([C || {C, _} <- WithCycles]),

            io:format("CO:~p~n", [CycleOnly]),
            case WithoutCycles of
                [] when CycleOnly /= undefined ->
                    {Xc, Mc} = CycleOnly,
                    %% Need to take at least enough steps to reach one value in each cyclic part.
                    MinimumSteps = lists:max([lists:min(Rs) || {{Rs, _}, _} <- WithCycles]),
                    lists:min([find_t(MinimumSteps, X, Mc) || X <- Xc]);
                _ ->
                    undefined
            end;
        Overlap ->
            lists:min(Overlap)
    end.

find_t(Min, X, M) when Min > X ->
    T = (Min - X) div M,
    case X + T * M of
        N when N < Min ->
            N + M;
        N ->
            N
    end;
find_t(_Min, X, _M) ->
    X.

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
            FirstVisit = maps:get({Pos, length(Path)}, Visited),
            Cycle = Count - FirstVisit,
            {{[G || G <- Goals, G >= FirstVisit], Cycle}, [G || G <- Goals, G < FirstVisit]}
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
