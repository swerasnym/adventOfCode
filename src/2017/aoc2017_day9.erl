-module(aoc2017_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "{}"}, star1, 1},
        {{data, "{{{}}},"}, star1, 6},
        {{data, "{{},{}}"}, star1, 5},
        {{data, "{{{},{},{{}}}}"}, star1, 16},
        {{data, "{<a>,<a>,<a>,<a>}"}, star1, 1},
        {{data, "{<a>,<a>,<a>,<a>}"}, star2, 4},
        {{data, "{{<ab>},{<ab>},{<ab>},{<ab>}}"}, star1, 9},
        {{data, "{{<ab>},{<ab>},{<ab>},{<ab>}}"}, star2, 8},
        {{data, "{{<!!>},{<!!>},{<!!>},{<!!>}}"}, star1, 9},
        {{data, "{{<!!>},{<!!>},{<!!>},{<!!>}}"}, star2, 0},
        {{data, "{{<a!>},{<a!>},{<a!>},{<ab>}}"}, star1, 3},
        {{data, "{{<a!>},{<a!>},{<a!>},{<ab>}}"}, star2, 17},

        {{data, "<>"}, star2, 0},
        {{data, "<<<<>"}, star2, 3},
        {{data, "<random characters>"}, star2, 17}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Stream) ->
    {Res, _} = group(0, 0, 0, Stream),
    Res.

star2(Stream) ->
    {_, Res} = group(0, 0, 0, Stream),
    Res.

read(File) ->
    tools:read_string(File).

group(0, Sum, Garbage, "") ->
    {Sum, Garbage};
group(D, Sum, Garbage, "<" ++ Rest) ->
    {Count, Cont} = remove_garbage(0, Rest),
    group(D, Sum, Garbage + Count, Cont);
group(D, Sum, Garbage, [Head | Rest]) ->
    case [Head] of
        "," -> group(D, Sum, Garbage, Rest);
        "{" -> group(D + 1, Sum, Garbage, Rest);
        "}" -> group(D - 1, Sum + D, Garbage, Rest)
    end.

remove_garbage(C, ">" ++ Rest) -> {C, Rest};
remove_garbage(C, "!" ++ Rest) -> remove_garbage(C, tl(Rest));
remove_garbage(C, Rest) -> remove_garbage(C + 1, tl(Rest)).
