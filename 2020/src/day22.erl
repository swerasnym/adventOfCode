-module(day22).

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

star1({P1, P2}) ->
    play_combat(P1, P2).

star2({P1, P2}) ->
    {_Player, Score} = play_recursive(P1, P2),
    Score.

read(File) ->
    {ok, Bin} = file:read_file(File),
    ["Player 1:\n" ++ P1, "Player 2:\n" ++ P2] =
        string:split(
            string:trim(binary_to_list(Bin)), "\n\n"),
    {[list_to_integer(Line) || Line <- string:split(P1, "\n", all)],
     [list_to_integer(Line) || Line <- string:split(P2, "\n", all)]}.

play_combat_round(P1, P2) when P1 > P2 ->
    {[P1, P2], []};
play_combat_round(P1, P2) ->
    {[], [P2, P1]}.

play_combat([], P2) ->
    score_combat(P2);
play_combat(P1, []) ->
    score_combat(P1);
play_combat([P1 | P1s], [P2 | P2s]) ->
    {T1, T2} = play_combat_round(P1, P2),
    play_combat(P1s ++ T1, P2s ++ T2).

score_combat(P) ->
    score_combat(P, length(P), 0).

score_combat([], 0, Sum) ->
    Sum;
score_combat([P | Ps], N, Sum) ->
    score_combat(Ps, N - 1, Sum + P * N).

play_recursive(P1, P2) ->
    play_recursive_round(P1, P2, sets:new()).

play_recursive_round(P1, [], _Set) ->
    {p1, score_combat(P1)};
play_recursive_round([], P2, _Set) ->
    {p2, score_combat(P2)};
play_recursive_round(P1 = [C1 | C1s], P2 = [C2 | C2s], Set) ->
    case sets:is_element({P1, P2}, Set) of
        true ->
            {p1, score_combat(P1)};
        false ->
            NewSet = sets:add_element({P1, P2}, Set),

            if C1 =< length(C1s), C2 =< length(C2s) ->
                   case play_recursive(lists:sublist(C1s, C1), lists:sublist(C2s, C2)) of
                       {p1, _Score} ->
                           play_recursive_round(C1s ++ [C1, C2], C2s, NewSet);
                       {p2, _Score} ->
                           play_recursive_round(C1s, C2s ++ [C2, C1], NewSet)
                   end;
               C1 > C2 ->
                   play_recursive_round(C1s ++ [C1, C2], C2s, NewSet);
               C2 > C1 ->
                   play_recursive_round(C1s, C2s ++ [C2, C1], NewSet)
            end
    end.
