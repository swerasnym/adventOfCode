-module(aoc2019_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({To, From}) ->
    Codes0 = lists:map(fun integer_to_list/1, lists:seq(To, From)),
    Codes1 = lists:filter(fun same/1, Codes0),
    Codes2 = lists:filter(fun non_dec/1, Codes1),
    length(Codes2).

star2({To, From}) ->
    Codes0 = lists:map(fun integer_to_list/1, lists:seq(To, From)),
    Codes1 = lists:filter(fun same2/1, Codes0),
    Codes2 = lists:filter(fun non_dec/1, Codes1),
    length(Codes2).

read(File) ->
    [[To, From]] = tools:read_multiple_formats(File, "~d-~d"),
    {To, From}.

same([A, A, _, _, _, _]) ->
    true;
same([_, A, A, _, _, _]) ->
    true;
same([_, _, A, A, _, _]) ->
    true;
same([_, _, _, A, A, _]) ->
    true;
same([_, _, _, _, A, A]) ->
    true;
same(_) ->
    false.

same2([A, A, B, _, _, _]) when B /= A ->
    true;
same2([C, A, A, B, _, _]) when B /= A andalso C /= A ->
    true;
same2([_, C, A, A, B, _]) when B /= A andalso C /= A ->
    true;
same2([_, _, C, A, A, B]) when B /= A andalso C /= A ->
    true;
same2([_, _, _, C, A, A]) when C /= A ->
    true;
same2(_) ->
    false.

non_dec([A | As]) ->
    non_dec(As, A).

non_dec([], _) ->
    true;
non_dec([A | _], Prev) when A < Prev ->
    false;
non_dec([A | As], _) ->
    non_dec(As, A).
