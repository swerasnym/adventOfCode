-module(day21).

-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
        star1 ->
            star1(Program);
        star2 ->
            star2(Program);
        interactive ->
            intcode:interactive(Program);
        _ ->
            Star1 = star1(Program),
            Star2 = star2(Program),
            {Star1, Star2}
    end.

star1(Program) ->
    Code = "OR A J\nAND B J\nAND C J\nNOT J J\nAND D J\nWALK\n",
    Result = intcode:run(Program, [{input, Code}]),
    Output = intcode:get_output(Result),

    io:format("~s~s", [Code, lists:droplast(Output)]),
    lists:last(Output).

star2(Program) ->
    Code =
        "OR A T\nAND B T\nAND C T\nNOT T T\nOR E J\nOR H J\nAND D J\nAND "
        "T J\nRUN\n",
    Result = intcode:run(Program, [{input, Code}]),
    Output = intcode:get_output(Result),

    io:format("~s~s", [Code, lists:droplast(Output)]),
    lists:last(Output).
