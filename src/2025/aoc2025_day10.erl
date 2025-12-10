-module(aoc2025_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day10_ex.txt", star1, 7},
        {"examples/2025/day10_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Machines) ->
    Solve = [min_presses(Wanted, Buttons) || {Wanted, Buttons, _} <- Machines],

    lists:sum(Solve).

star2(Data) ->
    Data,
    unknown.

read(File) ->
    tools:read_lines(File, fun parse_line/1).

parse_line(Line) ->
    [Wanted | ButtonsPowerS] = string:tokens(Line, "[] (){}"),
    ButtonsPower = [tools:parse_integers(T, ",") || T <- ButtonsPowerS],
    Power = lists:last(ButtonsPower),
    Buttons = lists:droplast(ButtonsPower),
    {Wanted, Buttons, Power}.

min_presses(Wanted, Buttons) ->
    io:format("~s, ~kp~n", [Wanted, Buttons]),
    Enumerated = lists:enumerate(0, Wanted),
    Start = #{I => $. || {I, _} <- Enumerated},
    End = #{I => S || {I, S} <- Enumerated},
    {value, P} = min_presses(Start, End, Buttons, 0),
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
