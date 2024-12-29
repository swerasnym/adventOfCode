-module(aoc2019_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Portals = maps:fold(fun portals/3, Data, Data),
    {Links, Start} = maps:fold(fun link/3, {Portals, none}, Portals),
    bfs([{Start, 0, [{"AA", 0}]}], Links).

star2(Data) ->
    Portals = maps:fold(fun portals/3, Data, Data),
    {Links, Start} = maps:fold(fun link/3, {Portals, none}, Portals),

    {Cx, Cy} =
        lists:max(maps:keys(maps:filter(fun(_Key, Value) -> Value == wall end, Links))),

    bfs2(
        [{Start, 0, 0, [{"AA", 0, 0}]}],
        #{
            0 => Links,
            original => Links,
            corner => {Cx + 1, Cy + 1}
        }
    ).

read(File) ->
    {ok, Bin} = file:read_file(File),
    List = binary_to_list(Bin),
    read(List, 0, 0, #{}).

read([], _X, _Y, Acc) ->
    Acc;
read([$\n | Rest], _X, Y, Acc) ->
    read(Rest, 0, Y + 1, Acc);
read([Char | Rest], X, Y, Acc) ->
    case Char of
        $# ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => wall});
        $. ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => open});
        PortalPart when $A =< PortalPart andalso $Z >= PortalPart ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => {portal_part, PortalPart}});
        _ ->
            read(Rest, X + 1, Y, Acc)
    end.

portals(Pos, {portal_part, P}, Acc) ->
    Neighbours =
        [{Res, NPos} || NPos <- neighbours(Pos), (Res = maps:get(NPos, Acc, none)) /= none],
    case Neighbours of
        [{open, _Opos}, {{portal_part, P2}, Ppos}] ->
            Acc1 = Acc#{Pos => {portal, [P, P2]}},
            maps:remove(Ppos, Acc1);
        [{{portal_part, P1}, Ppos}, {open, _Opos}] ->
            Acc1 = Acc#{Pos => {portal, [P1, P]}},
            maps:remove(Ppos, Acc1);
        _ ->
            Acc
    end;
portals(_Pos, _Value, Acc) ->
    Acc.

link(Pos, {portal, "AA"}, {Acc, _}) ->
    [Neighbour] = [NPos || NPos <- neighbours(Pos), maps:get(NPos, Acc, none) /= none],
    {Acc#{Pos => wall, Neighbour => start}, Neighbour};
link(Pos, {portal, "ZZ"}, {Acc, Start}) ->
    [Neighbour] = [NPos || NPos <- neighbours(Pos), maps:get(NPos, Acc, none) /= none],
    {Acc#{Pos => wall, Neighbour => goal}, Start};
link(_Pos, {portal, P2}, {Acc, Start}) ->
    Links =
        maps:filter(
            fun(_Key, Value) ->
                case Value of
                    {portal, P2} ->
                        true;
                    _ ->
                        false
                end
            end,
            Acc
        ),

    case maps:keys(Links) of
        [Pos1, Pos2] ->
            %% io:format("~p <-> ~p: ~p~n", [Pos1, Pos2, P2]),
            {Acc#{Pos1 => {Pos2, P2}, Pos2 => {Pos1, P2}}, Start};
        _ ->
            {Acc, Start}
    end;
link(_Pos, _Value, Acc) ->
    Acc.

neighbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}].

bfs([], _Maze) ->
    error(no_path);
bfs([{Pos, Dist, Path} | Rest], Maze) ->
    case maps:get(Pos, Maze) of
        wall ->
            bfs(Rest, Maze);
        goal ->
            io:format("path: ~p ~n", [lists:reverse([{"ZZ", Dist} | Path])]),
            Dist;
        {Pos2, Label} ->
            case [NPos || NPos <- neighbours(Pos2), maps:get(NPos, Maze, wall) /= wall] of
                [Neighbour] ->
                    bfs(
                        [{Neighbour, Dist, [{Label, Dist} | Path]} | Rest],
                        Maze#{Pos => wall, Pos2 => wall}
                    );
                [] ->
                    bfs(Rest, Maze)
            end;
        _ ->
            Neighbours =
                [
                    {NPos, Dist + 1, Path}
                 || NPos <- neighbours(Pos), maps:get(NPos, Maze, wall) /= wall
                ],
            bfs(Rest ++ Neighbours, Maze#{Pos => wall})
    end.

bfs2([], _Maze) ->
    error(no_path);
bfs2([{Pos, Dist, Level, Path} | Rest], Mazes) ->
    Maze = get_level(Mazes, Level),
    case maps:get(Pos, Maze) of
        wall ->
            bfs2(Rest, Mazes);
        goal ->
            case Level of
                0 ->
                    io:format("path: ~p ~n", [lists:reverse([{"ZZ", Dist, 0} | Path])]),
                    Dist;
                _ ->
                    Neighbours =
                        [
                            {NPos, Dist + 1, Level, Path}
                         || NPos <- neighbours(Pos), maps:get(NPos, Maze, wall) /= wall
                        ],
                    bfs2(Rest ++ Neighbours, Mazes#{Level => Maze#{Pos => wall}})
            end;
        {Pos2, Name} ->
            case next_level(Pos, Level, Mazes) of
                -1 ->
                    bfs2(Rest, Mazes);
                NextLevel ->
                    Maze2 = get_level(Mazes, NextLevel),
                    case [NPos || NPos <- neighbours(Pos2), maps:get(NPos, Maze2, wall) /= wall] of
                        [Neighbour] ->
                            bfs2(
                                [
                                    {Neighbour, Dist, NextLevel, [{Name, Dist, NextLevel} | Path]}
                                    | Rest
                                ],
                                Mazes#{
                                    Level => Maze#{Pos => wall},
                                    NextLevel => Maze2#{Pos2 => wall}
                                }
                            );
                        [] ->
                            bfs2(Rest, Mazes#{Level => Maze})
                    end
            end;
        _ ->
            Neighbours =
                [
                    {NPos, Dist + 1, Level, Path}
                 || NPos <- neighbours(Pos), maps:get(NPos, Maze, wall) /= wall
                ],
            bfs2(Rest ++ Neighbours, Mazes#{Level => Maze#{Pos => wall}})
    end.

get_level(Maps, Level) ->
    case maps:get(Level, Maps, new) of
        new ->
            maps:get(original, Maps);
        Map ->
            Map
    end.

next_level(Pos, Level, #{corner := {Cx, Cy}}) ->
    case Pos of
        {1, _} ->
            Level - 1;
        {_, 1} ->
            Level - 1;
        {Cx, _} ->
            Level - 1;
        {_, Cy} ->
            Level - 1;
        _ ->
            Level + 1
    end.
