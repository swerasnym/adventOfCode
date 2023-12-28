-module(aoc2021_day21).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {Turn, [_Winner, {_, _, Score}]} = play(Data, 0, 0),
    3 * Turn * Score.

star2([{1, Pos1, Score1}, {2, Pos2, Score2}]) ->
    erlang:erase(),
    erlang:put(dirac_roll, dirac_roll()),
    {W1, W2} = dirac_play({Pos1, Score1}, {Pos2, Score2}),
    max(W1, W2).

read(File) ->
    [
        {Pl, Pos, 0}
     || [Pl, Pos] <- tools:read_multiple_formats(File, "Player ~d starting position: ~d")
    ].

play([{Player, Pos, Score}, NextPlayer], Die, Turn) ->
    {Move, NextDie} = roll(Die),
    NextPos = (Pos + Move - 1) rem 10 + 1,
    NextScore = Score + NextPos,

    %% io:format("Player ~p rolls ~p:~p moves ~p score ~p~n",
    %%           [Player, Move, NextDie, NextPos,  NextScore] ),
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
    maps:to_list(tools:count([A + B + C || A <- R, B <- R, C <- R])).

dirac_play(_P1, {_Pos2, Score2}) when Score2 >= 21 ->
    {0, 1};
dirac_play({Pos, Score} = P1, P2) ->
    case erlang:get({P1, P2}) of
        undefined ->
            Results =
                [
                    begin
                        NextPos = (Pos + Roll - 1) rem 10 + 1,
                        {W2, W1} = dirac_play(P2, {NextPos, Score + NextPos}),
                        {M * W1, M * W2}
                    end
                 || {Roll, M} <- erlang:get(dirac_roll)
                ],

            Result = sum(Results, {0, 0}),
            erlang:put({P1, P2}, Result),
            Result;
        Result ->
            Result
    end.

sum([], Acc) ->
    Acc;
sum([{W1, W2} | Rest], {S1, S2}) ->
    sum(Rest, {W1 + S1, W2 + S2}).
