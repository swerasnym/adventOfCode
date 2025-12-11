-module(aoc2025_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day11_ex.txt", star1, 5},
        {"examples/2025/day11_ex2.txt", star2, 2}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 11},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Network) ->
    Start = "you",
    dfs(Start, Network).

star2(Network) ->
    Start = "svr",
    ToVisit = ["fft", "dac"],
    dfs(Start, Network, ToVisit).

read(File) ->
    maps:from_list(tools:read_lines(File, fun parse_device/1)).

parse_device(Line) ->
    [Device, Outputs] = string:split(Line, ": "),
    {Device, string:tokens(Outputs, " ")}.

dfs(Start, Network) ->
    dfs(Start, Network, []).

dfs(Start, Network, ToVisit) ->
    {_Visited, Result} = dfs(Start, Network, #{}, ToVisit),
    Result.

dfs("out", _Network, Visited, []) ->
    {Visited, 1};
dfs("out", _Network, Visited, _ToVisit) ->
    {Visited, 0};
dfs(Pos, Network, Visited, ToVisit) ->
    ToVisit1 = ToVisit -- [Pos],
    S = {Pos, ToVisit1},

    case maps:get(S, Visited, false) of
        false ->
            {ResV, ResS} = lists:foldl(
                fun(N, {VisitedAcc, ResAcc}) ->
                    {V1, Res1} = dfs(N, Network, VisitedAcc, ToVisit1),
                    {V1, ResAcc + Res1}
                end,
                {Visited#{S => 0}, 0},
                maps:get(Pos, Network)
            ),
            {ResV#{S => ResS}, ResS};
        Sum ->
            {Visited, Sum}
    end.
