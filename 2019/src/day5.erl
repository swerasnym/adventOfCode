-module(day5).

-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
        star1 ->
            star1(Program);
        star2 ->
            star2(Program);
        _ ->
            Star1 = star1(Program),
            Star2 = star2(Program),
            {Star1, Star2}
    end.

star1(Program) ->
    Result = intcode:run(Program, [{input, [1]}]),
    Output = intcode:get_output(Result),
    {lists:last(Output), lists:droplast(Output)}.

star2(Program) ->
    Result = intcode:run(Program, [{input, [5]}]),
    Output = intcode:get_output(Result),
    lists:last(Output).
