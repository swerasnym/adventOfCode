-module(aoc2023_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day18_ex.txt", star1, 62},
        {"examples/2023/day18_ex.txt", star2, 952408144115}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {Dirs, _} = lists:unzip(Data),
    {Dug, _} = lists:mapfoldl(fun dig/2, {0, 0}, Dirs),
    Map = maps:from_list(lists:flatten(Dug)),
    lists:sum([fill(L, 0, 0) || L <- tools:grid_to_lists(Map, $.)]).

star2(Data) ->
    Dirs = lists:map(fun change/1, Data),
    {Dug, {{0, 0}, P}} = lists:mapfoldl(fun dig2/2, {{0, 0}, 0}, Dirs),
    %% Assume non intersecting i.e. winding number 1.
    area([{0, 0} | Dug], 0) + P div 2 + 1.

read(File) ->
    [{{Dir, N}, Color} || [Dir, N, Color] <- tools:read_multiple_formats(File, "~c ~d~s")].

dig({"R", N}, {X, Y}) -> {[{{Xd, Y}, $R} || Xd <- lists:seq(X + 1, X + N)], {X + N, Y}};
dig({"L", N}, {X, Y}) -> {[{{Xd, Y}, $L} || Xd <- lists:seq(X - N, X - 1)], {X - N, Y}};
dig({"D", N}, {X, Y}) -> {[{{X, Yd}, $D} || Yd <- lists:seq(Y, Y + N)], {X, Y + N}};
dig({"U", N}, {X, Y}) -> {[{{X, Yd}, $U} || Yd <- lists:seq(Y - N, Y)], {X, Y - N}}.

%% Get ends
dig2({"R", N}, {{X, Y}, P}) -> {{X + N, Y}, {{X + N, Y}, P + N}};
dig2({"L", N}, {{X, Y}, P}) -> {{X - N, Y}, {{X - N, Y}, P + N}};
dig2({"D", N}, {{X, Y}, P}) -> {{X, Y + N}, {{X, Y + N}, P + N}};
dig2({"U", N}, {{X, Y}, P}) -> {{X, Y - N}, {{X, Y - N}, P + N}}.

area([_], Sum) ->
    Sum div 2;
area([{X1, Y1} | [{X2, Y2} | _] = Rest], Sum) ->
    area(Rest, Sum + (Y1 + Y2) * (X1 - X2)).

fill([], _, Acc) ->
    Acc;
fill([$U | Rest], _IO, Acc) ->
    fill(Rest, 1, Acc + 1);
fill([$D | Rest], _IO, Acc) ->
    fill(Rest, 0, Acc + 1);
fill([$L | Rest], IO, Acc) ->
    fill(Rest, IO, Acc + 1);
fill([$R | Rest], IO, Acc) ->
    fill(Rest, IO, Acc + 1);
fill([$. | Rest], IO, Acc) ->
    fill(Rest, IO, Acc + IO).

change({_, String}) ->
    [Instr] = string:tokens(String, "(#)"),
    Dir =
        case lists:last(Instr) - $0 of
            0 -> "R";
            1 -> "D";
            2 -> "L";
            3 -> "U"
        end,

    {Dir, erlang:list_to_integer(lists:sublist(Instr, 5), 16)}.
