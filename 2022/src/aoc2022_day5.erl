-module(aoc2022_day5).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day5.txt"),
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
    [B1, B2] = tools:read_blocks(File),
    G = tools:parse_grid(B1),
    RG = tools:rotate_grid(G, cw),
    Lists = tools:grid_to_lists(RG),
    Stacks1 = lists:filter(fun(L) -> hd(L) /= $  end, Lists),
    Stacks = [string:trim(tl(S)) || S <- Stacks1],
    Moves = tools:parse_format(B2, "move ~d from ~d to ~d\n"),

    {maps:from_list(lists:enumerate(Stacks)), Moves}.

star1({State, Moves}) ->
    Lists = maps:to_list(move(Moves, State)),
    [lists:last(S) || {_, S} <- lists:sort(Lists)].

star2({State, Moves}) ->
    Lists = maps:to_list(move9001(Moves, State)),
    [lists:last(S) || {_, S} <- lists:sort(Lists)].

move([], State) ->
    State;
move([[0, _, _] | Rest], State) ->
    move(Rest, State);
move([[N, F, T] | Rest], State) ->
    Lf = maps:get(F, State),
    Lt = maps:get(T, State),
    State1 = State#{F := lists:droplast(Lf), T := Lt ++ [lists:last(Lf)]},
    move([[N - 1, F, T] | Rest], State1).

move9001([], State) ->
    State;
move9001([[N, F, T] | Rest], State) ->
    Lf = maps:get(F, State),
    Lt = maps:get(T, State),
    {Head, Tail} = lists:split(length(Lf) - N, Lf),
    State1 = State#{F := Head, T := Lt ++ Tail},
    move9001(Rest, State1).
