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

-spec read(_) -> [any(), ...].
read(File) ->
    [B1, B2] = tools:read_blocks(File),
    Grid1 = string:replace(B1, "    ", "[ ] ", all),
    Grid2 = string:replace(Grid1, "]  [", "] [", all),
    Grid3 = string:replace(Grid2, "][", "] [", all),
    Grid4 = string:replace(Grid3, "] [", "", all),
    Grid5 = string:replace(Grid4, "]", "", all),
    Grid6 = string:replace(Grid5, "[", "", all),
    Grid7 = tools:parse_lines(Grid6),
    Grid8 = lists:droplast(Grid7),
    Grid = tools:lists_to_grid(Grid8),
    RGrid = tools:rotate_grid(Grid, cw),
    Lists = tools:grid_to_lists(RGrid),
    NoSpaces =
        [lists:filter(fun ($ ) ->
                              false;
                          (_) ->
                              true
                      end,
                      L)
         || L <- Lists],

    Moves =
        [begin
             [[C, T, F]] = tools:parse_format(L, "move ~d from ~d to ~d"),
             {C, T, F}
         end
         || L <- tools:parse_lines(B2)],
    {NoSpaces, Moves}.

star1({Stacks, Moves}) ->
    State =
        maps:from_list(
            lists:enumerate(Stacks)),
    Lists = maps:to_list(move(Moves, State)),
    [lists:last(S) || {_, S} <- lists:sort(Lists)].

star2({Stacks, Moves}) ->
    State =
        maps:from_list(
            lists:enumerate(Stacks)),
    Lists = maps:to_list(move9001(Moves, State)),
    [lists:last(S) || {_, S} <- lists:sort(Lists)].

move([], State) ->
    State;
move([{0, _, _} | Rest], State) ->
    move(Rest, State);
move([{N, F, T} | Rest], State) ->
    Lf = maps:get(F, State),
    Lt = maps:get(T, State),
    State1 = State#{F := lists:droplast(Lf), T := Lt ++ [lists:last(Lf)]},
    move([{N - 1, F, T} | Rest], State1).

move9001([], State) ->
    State;
move9001([{N, F, T} | Rest], State) ->
    Lf = maps:get(F, State),
    Lt = maps:get(T, State),
    {Head, Tail} = lists:split(length(Lf) - N, Lf),
    State1 = State#{F := Head, T := Lt ++ Tail},
    move9001(Rest, State1).
