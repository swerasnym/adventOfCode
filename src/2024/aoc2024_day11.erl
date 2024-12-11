-module(aoc2024_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day11_ex.txt", {star1, 6}, 22},
        {"examples/2024/day11_ex.txt", star1, 55312},
        {"examples/2024/day11_ex.txt", star2, 65601038650482}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 11},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Stones) ->
    star1(Stones, 25).

star1(Stones, N) ->
    blink(Stones, N).

star2(Stones) ->
    blink(Stones, 75).

read(File) ->
    tools:read_integers(File).

update(0) ->
    [1];
update(N) ->
    L = integer_to_list(N),
    case length(L) rem 2 of
        0 ->
            split(L);
        1 ->
            [N * 2024]
    end.

split(List) ->
    L = length(List),
    {A, B} = lists:split(L div 2, List),
    [list_to_integer(A), list_to_integer(B)].

blink(Stones, N) ->
    blink(Stones, N, #{}, 0).

blink([], N, Mem, Res) ->
    io:format("After ~p blinks, Memory size: ~p~n", [N, maps:size(Mem)]),
    Res;
blink([H | Rest], N, Mem, Res) ->
    {ResH, MemH} = blink(H, N, Mem),
    blink(Rest, N, MemH, Res + ResH).

blink(_, 0, Mem) ->
    {1, Mem};
blink(Num, Times, Mem) ->
    S = {Num, Times},
    case maps:get(S, Mem, first) of
        first ->
            case update(Num) of
                [A, B] ->
                    {Ra, MemA} = blink(A, Times - 1, Mem),
                    {Rb, MemB} = blink(B, Times - 1, MemA),
                    {Ra + Rb, MemB#{S => Ra + Rb}};
                [A] ->
                    %% Faster to not memorize in this case...
                    blink(A, Times - 1, Mem)
            end;
        Value ->
            {Value, Mem}
    end.
