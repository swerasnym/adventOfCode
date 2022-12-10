-module(aoc2022_day7).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day7.txt"),
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

read(File) ->
    tools:read_lines(File).

star1(["$ cd /" | Commands]) ->
    Files = run(Commands, ["/"], #{}),
    Dirs =
        maps:filter(fun ({dir, _}, _) ->
                            true;
                        (_, _) ->
                            false
                    end,
                    Files),
    %% io:format("~p~n", [Dirs]),
    Sizes = [sum_dir(K, Dirs) || K <- maps:keys(Dirs)],
    SmallDirs = lists:filter(fun(S) -> S =< 100000 end, Sizes),
    lists:sum(SmallDirs).

star2(["$ cd /" | Commands]) ->
    Files = run(Commands, ["/"], #{}),
    Dirs =
        maps:filter(fun ({dir, _}, _) ->
                            true;
                        (_, _) ->
                            false
                    end,
                    Files),
    %% io:format("~p~n", [Dirs]),
    Sizes = [sum_dir(K, Dirs) || K <- maps:keys(Dirs)],
    Used = sum_dir({dir, ["/"]}, Dirs),
    TotalSize = 70000000,
    NeededFree = 30000000,
    Free = TotalSize - Used,
    ToFree = NeededFree - Free,
    LargeDirs = lists:filter(fun(S) -> S >= ToFree end, Sizes),
    lists:min(LargeDirs).

run([], _Path, Files) ->
    Files;
run(["$ cd .." | Rest], Path, Files) ->
    run(Rest, tl(Path), Files);
run(["$ cd " ++ Dir | Rest], Path, Files) ->
    run(Rest, [Dir | Path], Files);
run(["$ ls" | Rest], Path, Files) ->
    {Commands, Files2} = ls(Rest, Path, Files, 0),
    run(Commands, Path, Files2).

ls([], Path, Files, Size) ->
    {[], Files#{{dir, lists:reverse(Path)} => Size}};
ls(Commands = ["$" ++ _ | _], Path, Files, Size) ->
    {Commands, Files#{{dir, lists:reverse(Path)} => Size}};
ls(["dir" ++ _ | Rest], Path, Files, Size) ->
    ls(Rest, Path, Files, Size);
ls([L | Rest], Path, Files, TotalSize) ->
    [[Size, Name]] = tools:parse_format(L, "~d ~s"),
    ls(Rest, Path, Files#{lists:reverse(Path, Name) => Size}, TotalSize + Size).

sum_dir({dir, Path}, Directories) ->
    SubDirs = maps:filter(fun({dir, D}, _) -> lists:prefix(Path, D) end, Directories),
    lists:sum(maps:values(SubDirs)).
