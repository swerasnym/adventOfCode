-module(aoc2019_day20).

-export([run/2]).

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

star1(Data) ->
    Portals = maps:fold(fun portals/3, Data, Data),
    {Links, Start} = maps:fold(fun link/3, {Portals, none}, Portals),
    bfs([{Start, 0, [{"AA", 0}]}], Links).

star2(Data) ->
    Portals = maps:fold(fun portals/3, Data, Data),
    {Links, Start} = maps:fold(fun link/3, {Portals, none}, Portals),

    {Cx, Cy} =
        lists:max(maps:keys(maps:filter(fun(_Key, Value) -> Value == wall end, Links))),

    bfs2([{Start, 0, 0, [{"AA", 0, 0}]}],
         #{0 => Links,
           original => Links,
           corner => {Cx + 1, Cy + 1}}).

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
    Neigbours =
        [{Res, NPos} || NPos <- neigbours(Pos), (Res = maps:get(NPos, Acc, none)) /= none],
    case Neigbours of
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
    [Neigbour] = [NPos || NPos <- neigbours(Pos), maps:get(NPos, Acc, none) /= none],
    {Acc#{Pos => wall, Neigbour => start}, Neigbour};
link(Pos, {portal, "ZZ"}, {Acc, Start}) ->
    [Neigbour] = [NPos || NPos <- neigbours(Pos), maps:get(NPos, Acc, none) /= none],
    {Acc#{Pos => wall, Neigbour => goal}, Start};
link(_Pos, {portal, P2}, {Acc, Start}) ->
    Links =
        maps:filter(fun(_Key, Value) ->
                       case Value of
                           {portal, P2} ->
                               true;
                           _ ->
                               false
                       end
                    end,
                    Acc),

    case maps:keys(Links) of
        [Pos1, Pos2] ->
            %% io:format("~p <-> ~p: ~p~n", [Pos1, Pos2, P2]),
            {Acc#{Pos1 => {Pos2, P2}, Pos2 => {Pos1, P2}}, Start};
        _ ->
            {Acc, Start}
    end;
link(_Pos, _Value, Acc) ->
    Acc.

neigbours({X, Y}) ->
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
            case [NPos || NPos <- neigbours(Pos2), maps:get(NPos, Maze, wall) /= wall] of
                [Neigbour] ->
                    bfs([{Neigbour, Dist, [{Label, Dist} | Path]} | Rest],
                        Maze#{Pos => wall, Pos2 => wall});
                [] ->
                    bfs(Rest, Maze)
            end;
        _ ->
            Neigbours =
                [{NPos, Dist + 1, Path}
                 || NPos <- neigbours(Pos), maps:get(NPos, Maze, wall) /= wall],
            bfs(Rest ++ Neigbours, Maze#{Pos => wall})
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
                    Neigbours =
                        [{NPos, Dist + 1, Level, Path}
                         || NPos <- neigbours(Pos), maps:get(NPos, Maze, wall) /= wall],
                    bfs2(Rest ++ Neigbours, Mazes#{Level => Maze#{Pos => wall}})
            end;
        {Pos2, Name} ->
            case next_level(Pos, Level, Mazes) of
                -1 ->
                    bfs2(Rest, Mazes);
                NextLevel ->
                    Maze2 = get_level(Mazes, NextLevel),
                    case [NPos || NPos <- neigbours(Pos2), maps:get(NPos, Maze2, wall) /= wall] of
                        [Neigbour] ->
                            bfs2([{Neigbour, Dist, NextLevel, [{Name, Dist, NextLevel} | Path]}
                                  | Rest],
                                 Mazes#{Level => Maze#{Pos => wall},
                                        NextLevel => Maze2#{Pos2 => wall}});
                        [] ->
                            bfs2(Rest, Mazes#{Level => Maze})
                    end
            end;
        _ ->
            Neigbours =
                [{NPos, Dist + 1, Level, Path}
                 || NPos <- neigbours(Pos), maps:get(NPos, Maze, wall) /= wall],
            bfs2(Rest ++ Neigbours, Mazes#{Level => Maze#{Pos => wall}})
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
