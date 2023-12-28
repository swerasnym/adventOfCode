-module(aoc2018_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day3_ex.txt", star1, 4},
        {"examples/2018/day3_ex.txt", star2, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Claims) ->
    ClaimedAreas = [
        {Left + W, Top + H}
     || {_, {Left, Top}, {With, Height}} <- Claims,
        W <- lists:seq(0, With - 1),
        H <- lists:seq(0, Height - 1)
    ],

    Counts = tools:count(ClaimedAreas),
    length([C || C <- maps:values(Counts), C > 1]).

star2(Claims) ->
    {NonOverlappingId, _, _} = no_overlaps(Claims, Claims),
    NonOverlappingId.

read(File) ->
    Lines = tools:read_format(File, "#~d @ ~d,~d: ~dx~d"),
    [{Claim, {Left, Top}, {With, Height}} || [Claim, Left, Top, With, Height] <- Lines].

no_overlaps([Claim | Rest], All) ->
    Overlaps = [overlap(Claim, C) || C <- All, C /= Claim],
    case lists:any(fun(V) -> V end, Overlaps) of
        true ->
            no_overlaps(Rest, All);
        false ->
            Claim
    end.

overlap({_, {L1, T1}, {W1, H1}}, {_, {L2, T2}, {W2, H2}}) ->
    tools:intervals_overlap({L1, L1 + W1}, {L2, L2 + W2}) andalso
        tools:intervals_overlap({T1, T1 + H1}, {T2, T2 + H2}).
