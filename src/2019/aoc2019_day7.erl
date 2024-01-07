-module(aoc2019_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2019/day7_1.data", star1, 43210},
        {"examples/2019/day7_2.data", star1, 54321},
        {"examples/2019/day7_3.data", star1, 65210},
        {"examples/2019/day7_4.data", star2, 139629729},
        {"examples/2019/day7_5.data", star2, 18216}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    {Res, P} = lists:max([
        {pashes(Program, Phases), Phases}
     || Phases <- tools:perms([0, 1, 2, 3, 4])
    ]),
    io:format("~p~n", [P]),
    Res.

star2(Program) ->
    {Res, P} = lists:max([{loop(Program, Phases), Phases} || Phases <- tools:perms([5, 6, 7, 8, 9])]),
    io:format("~p~n", [P]),
    Res.

read(File) ->
    intcode:from_file(File).

pashes(Program, Phases) ->
    pashes(Program, Phases, 0).

pashes(_, [], Input) ->
    Input;
pashes(Program, [Phase | Phases], Input) ->
    Result = intcode:run(Program, [{input, [Phase, Input]}]),
    [Output] = intcode:get_output(Result),
    pashes(Program, Phases, Output).

loop(Program, [P0, P1, P2, P3, P4]) ->
    F = fun(Input, Pid) ->
        Options = [{outputpid, Pid}, {input, [Input]}],
        intcode:spawn(Program, Options)
    end,

    Pid4 = F(P4, self()),
    Pid1 = lists:foldl(F, Pid4, [P3, P2, P1]),

    Options = [{outputpid, Pid1}, {input, [P0, 0]}],

    intcode:run(Program, Options),

    {halt, [Result]} = intcode:recvn(Pid4, all, 1000),
    Result.
