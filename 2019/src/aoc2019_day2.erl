-module(aoc2019_day2).

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
    Options = [{set, 12, 1}, {set, 2, 2}],
    Result = intcode:run(Program, Options),
    intcode:get(0, Result).

star2(Program) ->
    F = fun(Noun, Verb) ->
        Options = [{set, Noun, 1}, {set, Verb, 2}],
        Result = intcode:run(Program, Options),

        case intcode:get(0, Result) of
            19690720 ->
                done;
            _ ->
                continue
        end
    end,

    seek(F, 0, 0).

seek(_, _, 100) ->
    noresult;
seek(F, 100, V) ->
    seek(F, 0, V + 1);
seek(F, N, V) ->
    case F(N, V) of
        done ->
            100 * N + V;
        continue ->
            seek(F, N + 1, V)
    end.
