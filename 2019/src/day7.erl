-module(day7).
-export([run/2, pahses/2, perms/1, star1/1, loop/2]).

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
    lists:max([{pahses(Program, Phases), Phases} || Phases <- perms([0,1,2,3,4])]).

star2(Program) ->
    lists:max([{loop(Program, Phases), Phases} || Phases <- perms([5,6,7,8,9])]).

pahses(Program, Phases) ->
    pahses(Program, Phases, 0).

pahses(_, [], Input) ->
    Input;
pahses(Program, [Phase|Phases], Input) ->
    Program1 = intcode:set_input([Phase, Input], Program),
    Result = intcode:run(Program1),
    [Output] = intcode:get_output(Result),
    pahses(Program, Phases, Output).


loop(Program, [P0, P1, P2, P3, P4]) ->
     F = fun(Input, Pid) ->
		 Pr0 = intcode:set_output_pid(Pid, Program), 
		 Pr1 = intcode:set_input([Input], Pr0),
		 intcode:spawn(Pr1)
	 end,

    Pid4 = F(P4, self()),
    Pid1 = lists:foldl(F, Pid4, [P3, P2, P1]),
    
    Program0 = intcode:set_output_pid(Pid1, Program),
    Run = intcode:set_input([P0, 0], Program0),
    intcode:run(Run),

    case intcode:recvn(Pid4, all, 1000) of
	{halt, [Result]} ->
	    Result
    end.

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].
