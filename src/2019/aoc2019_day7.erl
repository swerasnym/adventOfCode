-module(aoc2019_day7).

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
    lists:max([{pahses(Program, Phases), Phases} || Phases <- perms([0, 1, 2, 3, 4])]).

star2(Program) ->
    lists:max([{loop(Program, Phases), Phases} || Phases <- perms([5, 6, 7, 8, 9])]).

pahses(Program, Phases) ->
    pahses(Program, Phases, 0).

pahses(_, [], Input) ->
    Input;
pahses(Program, [Phase | Phases], Input) ->
    Result = intcode:run(Program, [{input, [Phase, Input]}]),
    [Output] = intcode:get_output(Result),
    pahses(Program, Phases, Output).

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

perms([]) ->
    [[]];
perms(L) ->
    [[H | T] || H <- L, T <- perms(L -- [H])].
