-module(aoc2019_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

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

read(File) ->
    intcode:from_file(File).

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
