-module(aoc2017_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2017/day13_ex.txt", star1, 24},
        {"examples/2017/day13_ex.txt", star2, 10},
        {"examples/2017/day13_ex.txt", {star2, old}, 10}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 13},
        examples => Examples,
        all => [star1, star2, {star2, old}]
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Scanners) ->
    caught(0, Scanners, 0).

star2(Scanners) ->
    Acceptable = tools:group_kv([acceptable(S) || S <- Scanners]),
    Congruences = [{tools:overlap(Rems), Mod} || Mod := Rems <- Acceptable],
    {[Res | _], _} = tools:chinese_multi_reminder(Congruences),
    Res.

star2(Scanners, old) ->
    find_start(1, Scanners).

read(File) ->
    tools:group(2, tools:read_integers(File, " :\n")).

caught(_, [], Score) ->
    Score;
caught(S, [{Depth, Range} | Rest], Score) ->
    case tools:mod(S + Depth, cycle(Range)) of
        0 ->
            caught(S, Rest, Score + Depth * Range);
        _ ->
            caught(S, Rest, Score)
    end.

cycle(1) ->
    1;
cycle(N) when N > 1 ->
    2 * N - 2.

find_start(N, [{0, D} | _] = Scanners) ->
    case (N rem cycle(D) /= 0) andalso (caught(N, Scanners, 0) == 0) of
        true ->
            N;
        false ->
            find_start(N + 1, Scanners)
    end.

acceptable({Depth, Range}) ->
    C = cycle(Range),
    {C, lists:seq(0, C - 1) -- [tools:mod(-Depth, C)]}.
