-module(aoc2021_day21).

-export([run/2, profile/3, eprof/2]).

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

profile(Star, File, Times) ->
    Data = read(File),
    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1(Data) ->
    {Turn, [_Winner, {_, _, Score}]} = play(Data, 0, 0),
    3 * Turn * Score.

star2([{1, Pos1, Score1}, {2, Pos2, Score2}]) ->
    {W1, W2} = dirac_play([{Pos1, Score1}, {Pos2, Score2}, 0]),
    max(W1, W2).

%%    Data.

read(File) ->
    erase(),
    [{Pl, Pos, 0} || [Pl, Pos] <- tools:read_format(File, "Player ~d starting position: ~d")].

play([{Player, Pos, Score}, NextPlayer], Die, Turn) ->
    {Move, NextDie} = roll(Die),
    NextPos = (Pos + Move - 1) rem 10 + 1,
    NextScore = Score + NextPos,

    %% io:format("Player ~p rolls ~p:~p moves ~p score ~p~n", [Player, Move, NextDie, NextPos,  NextScore] ),
    case NextScore >= 1000 of
        true ->
            {Turn + 1, [{Player, NextPos, NextScore}, NextPlayer]};
        false ->
            play([NextPlayer, {Player, NextPos, NextScore}], NextDie, Turn + 1)
    end.

roll(Die) when Die < 98 ->
    {3 * Die + 6, Die + 3};
roll(100) ->
    {1 + 2 + 3, 3};
roll(99) ->
    {100 + 1 + 2, 2};
roll(98) ->
    {99 + 100 + 1, 1}.

dirac_roll() ->
    R = [1, 2, 3],
    maps:to_list(
        tools:count([A + B + C || A <- R, B <- R, C <- R])).

dirac_play([{Pos, Score}, P2, 3]) ->
    {W2, W1} = dirac_play([P2, {Pos, Score + Pos}, 0]),
    {W1, W2};
dirac_play([{_Pos1, Score1}, {_Pos2, _Score2}, _Turn]) when Score1 >= 21 ->
    {1, 0};
dirac_play([{_Pos1, _Score1}, {_Pos2, Score2}, _Turn]) when Score2 >= 21 ->
    {0, 1};
dirac_play([{Pos, Score}, {_Pos2, _Score2} = P2, Turn] = State) ->
    case get(State) of
        undefined ->
            Results =
                [begin
                     NextPos = (Pos + Roll - 1) rem 10 + 1,
                     {_W1, _W2} = dirac_play([{NextPos, Score}, P2, Turn + 1])
                 end
                 || Roll <- [1, 2, 3]],

            Result = sum(Results, {0, 0}),
            put(State, Result),
            Result;
        Result ->
            Result
    end.

sum([], Acc) ->
    Acc;
sum([{W1, W2} | Rest], {S1, S2}) ->
    sum(Rest, {W1 + S1, W2 + S2}).
