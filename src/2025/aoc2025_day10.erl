-module(aoc2025_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day10_ex.txt", star1, 7},
        {"examples/2025/day10_ex.txt", star2, 33}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 10},
        unit_test_input => false,
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Machines) ->
    Solve = [min_presses(Wanted, Buttons) || {Wanted, Buttons, _} <- Machines],

    lists:sum(Solve).

star2(Machines) ->
    Solve = [min_presses2(Wanted, Buttons) || {_, Buttons, Wanted} <- Machines],

    lists:sum(Solve).

read(File) ->
    tools:read_lines(File, fun parse_line/1).

parse_line(Line) ->
    [Wanted | ButtonsPowerS] = string:tokens(Line, "[] (){}"),
    ButtonsPower = [tools:parse_integers(T, ",") || T <- ButtonsPowerS],
    Power = lists:last(ButtonsPower),
    Buttons = lists:droplast(ButtonsPower),
    {Wanted, Buttons, Power}.

min_presses(Wanted, Buttons) ->
    Sorted = tools:reverse_sort([{length(B), B} || B <- Buttons]),
    ButtonsIn = [B || {_, B} <- Sorted],
    % io:format("~s, ~kp~n", [Wanted, ButtonsIn]),

    Enumerated = lists:enumerate(0, Wanted),
    Start = #{I => $. || {I, _} <- Enumerated},
    End = #{I => S || {I, S} <- Enumerated},
    {value, P} = min_presses(Start, End, ButtonsIn, 0),
    P.

min_presses(End, End, _, P) ->
    {value, P};
min_presses(_, _, [], _) ->
    false;
min_presses(State, End, [Button | Rest], P) ->
    A = min_presses(press(State, Button), End, Rest, P + 1),
    B = min_presses(State, End, Rest, P),
    case {A, B} of
        {false, Rb} -> Rb;
        {Ra, false} -> Ra;
        {{value, Ra}, {value, Rb}} -> {value, min(Ra, Rb)}
    end.

press(State, Button) ->
    maps:merge(State, #{I => toggle(maps:get(I, State)) || I <- Button}).

toggle($.) -> $#;
toggle($#) -> $..

min_presses2(Wanted, Buttons) ->
    Enumerated = lists:enumerate(0, Wanted),
    Start = #{I => 0 || {I, _} <- Enumerated},
    End = #{I => S || {I, S} <- Enumerated},
    % io:format("~kp ~kp ---, ~kp~n", [Start, End, Buttons]),
    {_, {value, P}} = min_presses2(Start, End, Buttons, #{}),
    P.
min_presses2(over, _, _, Mem) ->
    {Mem, false};
min_presses2(End, End, _, Mem) ->
    {Mem, {value, 0}};
min_presses2(_, _, [], Mem) ->
    {Mem, false};
min_presses2(State, End, [Button | Rest] = All, Mem) ->
    S = {length(All), State},
    case maps:get(S, Mem, unknown) of
        _ ->
            {MemA, A} = min_presses2(press2(State, Button, End), End, All, Mem),
            {MemB, B} = min_presses2(State, End, Rest, MemA),
            Return =
                case {A, B} of
                    {false, Rb} -> Rb;
                    {{value, Ra}, false} -> {value, Ra + 1};
                    {{value, Ra}, {value, Rb}} -> {value, min(Ra + 1, Rb)}
                end,

            {Mem, Return};
        Value ->
            {Mem, Value}
    end.

press2(State, Button, End) ->
    New = #{I => increase(maps:get(I, State)) || I <- Button},
    % io:format("~kp~n", [New]),
    case over(maps:to_list(New), End) of
        true -> over;
        false -> maps:merge(State, New)
    end.

increase(V) -> V + 1.

over([], _) ->
    false;
over([{K, V} | Rest], End) ->
    case V > maps:get(K, End) of
        true -> true;
        false -> over(Rest, End)
    end.
