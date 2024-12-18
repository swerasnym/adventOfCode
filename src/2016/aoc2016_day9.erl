-module(aoc2016_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "ADVENT"}, star1, 6},
        {{data, "A(1x5)BC"}, star1, 7},
        {{data, "(3x3)XYZ"}, star1, 9},
        {{data, "(3x3)XYZ"}, star2, 9},
        {{data, "(6x1)(1x3)A"}, star1, 6},
        {{data, "X(8x2)(3x3)ABCD"}, star1, 18},
        {{data, "X(8x2)(3x3)ABCD"}, star2, 20},
        {{data, "X(8x2)(3x3)ABCD"}, star2, 20},
        {{data, "(27x12)(20x12)(13x14)(7x10)(1x12)A"}, star2, 241920},
        {{data, "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"}, star2, 445}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(String) ->
    length(decompress(String, [])).

star2(String) ->
    decompress2(String, 0).

read(File) ->
    tools:read_string(File).

decompress("", Out) ->
    lists:flatten(Out);
decompress(String, Out) ->
    case string:split(String, "(") of
        [Head, MarkTail] ->
            [Mark, Tail] = string:split(MarkTail, ")"),
            [Chars, Times] = tools:parse_format(Mark, "~dx~d"),
            {Block, Rest} = lists:split(Chars, Tail),
            decompress(Rest, Out ++ Head ++ lists:duplicate(Times, Block));
        [String] ->
            lists:flatten(Out ++ String)
    end.

decompress2("", Out) ->
    Out;
decompress2(String, Out) ->
    case string:split(String, "(") of
        [Head, MarkTail] ->
            [Mark, Tail] = string:split(MarkTail, ")"),
            [Chars, Times] = tools:parse_format(Mark, "~dx~d"),
            {Block, Rest} = lists:split(Chars, Tail),
            R = decompress2(Block, 0),

            decompress2(Rest, Out + length(Head) + R * Times);
        [String] ->
            Out + length(String)
    end.
