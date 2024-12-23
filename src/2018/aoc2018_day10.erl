-module(aoc2018_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day10_ex.txt", star1, "HI"},
        {"examples/2018/day10_ex.txt", star2, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Stars) ->
    First = hd(Stars),
    Converted = [subtract(S, First) || S <- Stars],
    % io:format("~p~n", [Converted]),
    Close = [close_to_zero(S) || S <- Converted],
    %  io:format("~p~n", [Close]),
    Times = tools:overlap([C || C <- Close, C /= always]),

    print_map(lists:last(Times), Converted).

star2(Stars) ->
    First = hd(Stars),
    Converted = [subtract(S, First) || S <- Stars],
    % io:format("~p~n", [Converted]),
    Close = [close_to_zero(S) || S <- Converted],
    %  io:format("~p~n", [Close]),
    Times = tools:overlap([C || C <- Close, C /= always]),
    lists:last(Times).
read(File) ->
    Read = tools:read_multiple_formats(File, "position=<~d, ~d> velocity=<~d, ~d>"),
    [{{X, Y}, {Vx, Vy}} || [X, Y, Vx, Vy] <- Read].

subtract({{X1, Y1}, {Vx1, Vy1}}, {{X, Y}, {Vx, Vy}}) ->
    {{X1 - X, Y1 - Y}, {Vx1 - Vx, Vy1 - Vy}}.

close_to_zero({_, {_, 0}}) ->
    always;
close_to_zero({{_, Y}, {_, Vy}}) ->
    Start = -Y div Vy,

    [
        T
     || T <- lists:seq(Start - 10, Start + 10),
        T > 0,
        Y + Vy * T > -10,
        Y + Vy * T < 10
    ].

print_map(T, Stars) ->
    Map = #{{X + Vx * T, Y + Vy * T} => $█ || {{X, Y}, {Vx, Vy}} <- Stars},
    io:format("After ~p seconds:~n", [T]),
    %io:format("~p", [Map]),
    tools:print_grid(Map),
    io:nl(),
    aoc_ocr:decode(Map, $█).
