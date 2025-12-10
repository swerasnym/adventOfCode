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
    io:format("~s~n", [lists:duplicate(length(Machines), $-)]),
    Solve = [solve(Wanted, Buttons) || {_, Buttons, Wanted} <- Machines],
    io:format("~n"),
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

press(Button, State, Times) ->
    maps:merge(State, #{I => maps:get(I, State) - Times || I <- Button}).

not_harmful(Button, State) ->
    lists:min(maps:values(press(Button, State, 1))) >= 0.

useful_buttons(State, Buttons) ->
    KV = maps:to_list(State),
    [useful_buttons(L, H, Buttons, HV - LV) || {L, LV} <- KV, {H, HV} <- KV, LV < HV].

useful_buttons(Lower, Higher, Buttons, Diff) ->
    [
        {Diff, Button}
     || Button <- Buttons, lists:member(Higher, Button) andalso not lists:member(Lower, Button)
    ].

solve(Wanted, Buttons) ->
    Enumerated = lists:enumerate(0, Wanted),
    End = #{I => S || {I, S} <- Enumerated},
    Sum = lists:sum(Wanted),
    io:format("."),
    solve(End, Buttons, 0, Sum, Sum).

solve(_State, _Buttons, Presses, MinPresses, Sum) when Presses >= MinPresses orelse Sum < 0 ->
    % We have passed the best known solution, no need to continue.
    MinPresses;
solve(State, _Buttons, Presses, MinPresses, 0) ->
    case lists:min(maps:values(State)) >= 0 of
        true -> Presses;
        false -> MinPresses
    end;
solve(_State, [], _Presses, MinPresses, _Sum) ->
    % no buttons left to press, with things left to press...
    MinPresses;
solve(State, Buttons, Presses, MinPresses, Sum) ->
    true = Sum > 0,
    Buttons1 = lists:filter(fun(B) -> not_harmful(B, State) end, Buttons),
    Useful = useful_buttons(State, Buttons1),
    Single = lists:filter(fun single/1, Useful),
    case lists:member([], Useful) of
        true ->
            MinPresses;
        false when length(Single) > 0 ->
            [{Times, Button}] = lists:max(Single),
            solve(
                press(Button, State, Times),
                Buttons1,
                Presses + Times,
                MinPresses,
                Sum - length(Button) * Times
            );
        false ->
            one_press(State, Buttons1, Presses, MinPresses, Sum)
    end.

one_press(State, [Button | Rest] = Buttons, Presses, MinPresses, Sum) ->
    Min1 = solve(press(Button, State, 1), Buttons, Presses + 1, MinPresses, Sum - length(Button)),
    one_press(State, Rest, Presses, Min1, Sum);
one_press(_State, [], _Presses, MinPresses, _Sum) ->
    MinPresses.

single([_]) -> true;
single(_) -> false.
