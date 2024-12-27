-module(aoc2017_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day1_ex.txt", star1, 9},
        {"examples/2017/day1_ex.txt", star2, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Sequence) ->
    captcha1(Sequence ++ [hd(Sequence)], 0).

star2(Sequence) ->
    {H1, H2} = lists:split(length(Sequence) div 2, Sequence),
    captcha2(H1, H2, 0).

read(File) ->
    tools:read_string(File).

captcha1([_], Sum) ->
    Sum;
captcha1([A | [A | _] = Rest], Sum) ->
    captcha1(Rest, Sum + A - $0);
captcha1([_ | Rest], Sum) ->
    captcha1(Rest, Sum).

captcha2([], [], Sum) ->
    Sum;
captcha2([A | Rest], [A | Rest2], Sum) ->
    captcha2(Rest, Rest2, Sum + (A - $0) * 2);
captcha2([_ | Rest], [_ | Rest2], Sum) ->
    captcha2(Rest, Rest2, Sum).
