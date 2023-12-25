-module(aoc2020_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 22},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({P1, P2}) ->
    play(P1, P2).

star2({P1, P2}) ->
    {_Player, Score} = play_recursive(P1, P2),
    Score.

read(File) ->
    ["Player 1:\n" ++ P1, "Player 2:\n" ++ P2] = tools:read_blocks(File),
    {tools:parse_integers(P1), tools:parse_integers(P2)}.

play_round(P1, P2) when P1 > P2 ->
    {[P1, P2], []};
play_round(P1, P2) ->
    {[], [P2, P1]}.

play([], P2) ->
    score(P2);
play(P1, []) ->
    score(P1);
play([P1 | P1s], [P2 | P2s]) ->
    {T1, T2} = play_round(P1, P2),
    play(P1s ++ T1, P2s ++ T2).

score(P) ->
    score(P, length(P), 0).

score([], 0, Sum) ->
    Sum;
score([P | Ps], N, Sum) ->
    score(Ps, N - 1, Sum + P * N).

play_recursive(P1, P2) ->
    play_recursive_round(P1, P2, sets:new()).

play_recursive_round(P1, [], _Set) ->
    {p1, score(P1)};
play_recursive_round([], P2, _Set) ->
    {p2, score(P2)};
play_recursive_round([C1 | C1s] = P1, [C2 | C2s] = P2, Set) ->
    case sets:is_element({P1, P2}, Set) of
        true ->
            {p1, score(P1)};
        false ->
            NewSet = sets:add_element({P1, P2}, Set),

            case {C1, C2} of
                {C1, C2} when C1 =< length(C1s), C2 =< length(C2s) ->
                    case play_recursive(lists:sublist(C1s, C1), lists:sublist(C2s, C2)) of
                        {p1, _Score} ->
                            play_recursive_round(C1s ++ [C1, C2], C2s, NewSet);
                        {p2, _Score} ->
                            play_recursive_round(C1s, C2s ++ [C2, C1], NewSet)
                    end;
                {C1, C2} when C1 > C2 ->
                    play_recursive_round(C1s ++ [C1, C2], C2s, NewSet);
                {C1, C2} when C2 > C1 ->
                    play_recursive_round(C1s, C2s ++ [C2, C1], NewSet)
            end
    end.
