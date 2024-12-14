-module(aoc2015_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day23_ex.txt", star1, 2}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    Regs = exec(#{a => 0, b => 0}, 0, Program),
    io:format("Exit: ~kp~n", [Regs]),
    maps:get(b, Regs).

star2(Program) ->
    Regs = exec(#{a => 1, b => 0}, 0, Program),
    io:format("Exit: ~kp~n", [Regs]),
    maps:get(b, Regs).

read(File) ->
    maps:from_list(lists:enumerate(0, tools:read_lines(File, fun parse_instruction/1))).

parse_instruction("hlf " ++ Reg) ->
    {hlf, list_to_atom(Reg)};
parse_instruction("tpl " ++ Reg) ->
    {tpl, list_to_atom(Reg)};
parse_instruction("inc " ++ Reg) ->
    {inc, list_to_atom(Reg)};
parse_instruction("jmp " ++ Rest) ->
    [Offset] = tools:parse_format(Rest, "~d"),
    {jmp, Offset};
parse_instruction("jie " ++ [Reg | Rest]) ->
    [Offset] = tools:parse_format(Rest, ", ~d"),
    {jie, list_to_atom([Reg]), Offset};
parse_instruction("jio " ++ [Reg | Rest]) ->
    [Offset] = tools:parse_format(Rest, ", ~d"),
    {jio, list_to_atom([Reg]), Offset}.

exec(Regs, SP, Program) ->
    Instr = maps:get(SP, Program, exit),

    case Instr of
        exit ->
            Regs;
        {hlf, R} ->
            exec(Regs#{R => maps:get(R, Regs) div 2}, SP + 1, Program);
        {tpl, R} ->
            exec(Regs#{R => maps:get(R, Regs) * 3}, SP + 1, Program);
        {inc, R} ->
            exec(Regs#{R => maps:get(R, Regs) + 1}, SP + 1, Program);
        {jmp, Offset} ->
            exec(Regs, SP + Offset, Program);
        {jie, R, Offset} when map_get(R, Regs) rem 2 == 0 ->
            exec(Regs, SP + Offset, Program);
        {jie, _, _} ->
            exec(Regs, SP + 1, Program);
        {jio, R, Offset} when map_get(R, Regs) == 1 ->
            exec(Regs, SP + Offset, Program);
        {jio, _, _} ->
            exec(Regs, SP + 1, Program)
    end.
