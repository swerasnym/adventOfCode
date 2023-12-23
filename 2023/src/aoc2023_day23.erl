-module(aoc2023_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day23_ex.txt", star1, 94},
        {"2023/data/day23_ex.txt", star2, 154}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(#{max := {Xmax, Ymax}} = Grid) ->
    erlang:erase(),
    Start = {1, 0},
    End = {Xmax - 1, Ymax},

    % tools:print_grid(Grid),
    dfs(Start, End, Grid, 0, fun valid1/2).

% not 5102
% not 5910
% > 6374
% > 6498
star2(#{max := {Xmax, Ymax}} = Grid) ->
    Multiways = [{X, Y} || {X, Y} := V <- Grid, V /= $#, multiway({X, Y}, Grid)],
    End = {Xmax - 1, Ymax},
    Start = {1, 0},

    %     simplify(Start, [End | Multiways], 0, Start, Grid).

    Compressed = [
        {P, simplify(P, [Start, End | Multiways], 0, P, Grid)}
     || P <- [Start | Multiways]
    ],
    CGrid = maps:from_list(Compressed),
    dfs2(Start, End, CGrid, 0, []).

% erlang:erase(),
% Start = {1, 0},
% End = {Xmax - 1, Ymax},

% tools:print_grid(Grid),
% dfs(Start, End, Grid, 0, fun valid2/2).

read(File) ->
    tools:read_grid(File).

simplify(Pos, Ends, 0, Last, Grid) ->
    io:format("~p~n", [Pos]),
    Dirs = [north, west, east, south],
    Next = [get_neigbour(Pos, Dir) || Dir <- Dirs],
    NextPs = [NP || NP <- Next, maps:get(NP, Grid, $#) /= $#] -- [Last],
    [simplify(NP, Ends, 1, Pos, Grid) || NP <- NextPs];
simplify(Pos, Ends, Steps, Last, Grid) ->
    case lists:member(Pos, Ends) of
        true ->
            {Pos, Steps};
        false ->
            Dirs = [north, west, east, south],
            Next = [get_neigbour(Pos, Dir) || Dir <- Dirs],
            [NextP] = [NP || NP <- Next, maps:get(NP, Grid, $#) /= $#] -- [Last],
            simplify(NextP, Ends, Steps + 1, Pos, Grid)
    end.

dfs2(End, End, _Grid, Steps, _Path) ->
    Steps;
dfs2(Pos, End, Grid, Steps, Path) ->
    NextPs = [{NP, S} || {NP, S} <- maps:get(Pos, Grid), false == lists:member(NP, Path)],

    case NextPs of
        [] ->
            0;
        _ ->
            lists:max([dfs2(NP, End, Grid, Steps + S, [Pos | Path]) || {NP, S} <- NextPs])
    end.

dfs(End, End, Grid, Steps, _) ->
    case erlang:get(e) of
        undefined ->
            erlang:put(e, Steps),
            io:format("~p: ~p~n", [Steps, End]);
        N when N < Steps ->
            erlang:put(e, Steps),
            io:format("~p: ~p~n", [Steps, End]);
        _ ->
            ok
    end,

    %  tools:print_grid(Grid#{End := $E}),
    Steps;
dfs(Pos, End, Grid, Steps, Valid) ->
    Key = {Pos, Steps},
    case erlang:get(Key) of
        undefined ->
            % erlang:put(Key, visited),

            Dirs = [north, west, east, south],

            Next = [{Dir, get_neigbour(Pos, Dir)} || Dir <- Dirs],
            NextP = [NP || {Dir, NP} <- Next, Valid(Dir, maps:get(NP, Grid, $#))],

            % case NextP of
            %     [] ->
            %         io:format("X ~p: ~p~n", [Steps, Pos]);
            %     %   tools:print_grid(Grid#{Pos := $P});
            %     _ ->
            %         ok
            % end,

            NextV =
                lists:usort(
                    [dfs(NP, End, Grid#{Pos := $O}, Steps + 1, Valid) || NP <- NextP]
                ),

            case NextP of
                [] ->
                    0;
                _ ->
                    Res = lists:max(NextV),
                    % erlang:put(Key, Res),
                    Res
            end;
        N ->
            N
    end.

multiway(Pos, Grid) ->
    Dirs = [north, west, east, south],
    Next = [get_neigbour(Pos, Dir) || Dir <- Dirs],
    NextP = [NP || NP <- Next, maps:get(NP, Grid, $#) /= $#],
    length(NextP) > 2.

get_neigbour({X, Y}, north) -> {X, Y - 1};
get_neigbour({X, Y}, south) -> {X, Y + 1};
get_neigbour({X, Y}, east) -> {X + 1, Y};
get_neigbour({X, Y}, west) -> {X - 1, Y}.

valid1(_Dir, $.) -> true;
valid1(_Dir, $#) -> false;
valid1(_Dir, $O) -> false;
valid1(north, $^) -> true;
valid1(south, $v) -> true;
valid1(east, $>) -> true;
valid1(west, $<) -> true;
valid1(_Dir, _) -> false.
