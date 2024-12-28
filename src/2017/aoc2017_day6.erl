-module(aoc2017_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day6_ex.txt", star1, 5},
        {"examples/2017/day6_ex.txt", star2, 4}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Banks0) ->
    {N, _, End, _} = tools:find_cycle(reallocate(maps:size(Banks0)), Banks0),
    io:format("~p~n", [End]),
    N.

star2(Banks0) ->
    {_, Length, _, _} = tools:find_cycle(reallocate(maps:size(Banks0)), Banks0),
    Length.

read(File) ->
    maps:from_list(lists:enumerate(0, tools:read_integers(File))).

reallocate(N) ->
    fun(Banks) ->
        {Blocks, _, Idx} = lists:max([{B, -I, I} || I := B <- Banks]),
        All = Blocks div N,
        Rem = Blocks rem N,
        Banks1 = #{I => B + All + bonus(I, Rem, Idx, N) || I := B <- Banks, I /= Idx},
        Banks1#{Idx => All}
    end.

bonus(Idx, _, Idx, _) ->
    0;
bonus(_, 0, _, _) ->
    0;
bonus(I, Rem, Idx, N) ->
    case I > Idx of
        true when I =< Rem + Idx ->
            1;
        false when I + N =< Rem + Idx ->
            1;
        _ ->
            0
    end.
