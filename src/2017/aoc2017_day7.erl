-module(aoc2017_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day7_ex.txt", star1, "tknk"},
        {"examples/2017/day7_ex.txt", star2, 60}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Programs) ->
    SubPrograms = [P || _ := {_, Sps} <- Programs, P <- Sps],
    [Bottom] = maps:keys(Programs) -- SubPrograms,
    Bottom.

star2(Programs) ->
    SubPrograms = [P || _ := {_, Sps} <- Programs, P <- Sps],
    [Bottom] = maps:keys(Programs) -- SubPrograms,
    Bottom,
    find_new(0, Bottom, Programs).

read(File) ->
    maps:from_list(tools:read_lines(File, fun parse_state/1)).

parse_state(State) ->
    [H | R] = string:split(State, " -> "),
    [Name, Weight] = tools:parse_format(H, "~s (~d)"),
    case R of
        [] ->
            {Name, {Weight, []}};
        [List] ->
            {Name, {Weight, string:split(List, ", ", all)}}
    end.

find_new(Diff, R, Programs) ->
    {ThisW, Sp} = maps:get(R, Programs),
    Weights = #{S => weight(S, Programs) || S <- Sp},
    Counts = [{C, W} || W := C <- tools:count(Weights)],
    case tools:min_or(Counts, this) of
        {1, Wrong} ->
            [Strange] = [S || S := W <- Weights, W == Wrong],
            {_, Rest} = lists:max(Counts),
            find_new(Wrong - Rest, Strange, Programs);
        _ ->
            io:format("~s should way: ~p~n", [R, ThisW - Diff]),
            ThisW - Diff
    end.

weight(P, Programs) ->
    {W, Sp} = maps:get(P, Programs),
    W + lists:sum([weight(S, Programs) || S <- Sp]).
