-module(aoc_watch_code).

%% Defined to take 3 parameters (in addition to state) in problem description.
-hank([{unnecessary_function_arguments, [{seti, 4}]}]).
-hank([{unnecessary_function_arguments, [{setr, 4}]}]).

-export([new/0]).
-export([new/1]).
-export([new/2]).
-export([set_registers/2]).
-export([get_registers/1]).
-export([instructions_map/0]).
-export([execute/2]).
-export([set_ip_reg/2]).
-export([load_program/2]).
-export([run/1]).
-export([set_reg/3]).
-export([get_reg/2]).
-export([run_dbg/2]).

-record(state, {reg, reg_size, ip_reg = 5, program = #{}}).

new() ->
    new(6).

new(List) when is_list(List) ->
    set_registers(new(length(List)), List);
new(N) ->
    #state{reg = array:new(N, [{fixed, true}, {default, 0}]), reg_size = N}.

new(N, IpReg) ->
    set_ip_reg(new(N), IpReg).

set_registers(#state{reg_size = N} = S, Values) when length(Values) == N ->
    S#state{reg = array:fix(array:from_list(Values))}.

get_registers(#state{reg = Reg}) ->
    array:to_list(Reg).

set_ip_reg(S, IpReg) ->
    set_reg(S#state{ip_reg = IpReg}, IpReg, 0).

load_program(S, Program) when is_list(Program) ->
    S#state{program = maps:from_list(lists:enumerate(0, Program))}.

run(#state{ip_reg = IpReg, program = Program} = S) ->
    Line = get_reg(S, IpReg),
    case maps:get(Line, Program, halt) of
        halt ->
            S;
        Instructions ->
            S1 = execute(S, Instructions),
            run(addi(S1, IpReg, 1, IpReg))
    end.

run_dbg(S, 0) ->
    io:format("halt~n"),
    S;
run_dbg(#state{ip_reg = IpReg, program = Program} = S, N) ->
    Line = get_reg(S, IpReg),
    case maps:get(Line, Program, halt) of
        halt ->
            S;
        Instructions ->
            io:format("IP: ~p\t Inst: ~0p \t Regs: ~0p ~n", [Line, Instructions, get_registers(S)]),

            S1 = execute(S, Instructions),
            run_dbg(addi(S1, IpReg, 1, IpReg), N - 1)
    end.

execute(S, [Instruction, A, B, C]) when is_atom(Instruction) ->
    Fun = maps:get(Instruction, instructions_map()),
    Fun(S, A, B, C).

instructions_map() ->
    #{
        addr => fun addr/4,
        addi => fun addi/4,
        mulr => fun mulr/4,
        muli => fun muli/4,
        banr => fun banr/4,
        bani => fun bani/4,
        borr => fun borr/4,
        bori => fun bori/4,
        setr => fun setr/4,
        seti => fun seti/4,
        gtir => fun gtir/4,
        gtri => fun gtri/4,
        gtrr => fun gtrr/4,
        eqir => fun eqir/4,
        eqri => fun eqri/4,
        eqrr => fun eqrr/4
    }.

get_reg(#state{reg = Reg}, I) ->
    array:get(I, Reg).

set_reg(#state{reg = Reg} = S, I, V) ->
    S#state{reg = array:set(I, V, Reg)}.

addr(S, A, B, C) ->
    Va = get_reg(S, A),
    Vb = get_reg(S, B),
    set_reg(S, C, Va + Vb).

addi(S, A, Vb, C) ->
    Va = get_reg(S, A),
    set_reg(S, C, Va + Vb).

mulr(S, A, B, C) ->
    Va = get_reg(S, A),
    Vb = get_reg(S, B),
    set_reg(S, C, Va * Vb).

muli(S, A, Vb, C) ->
    Va = get_reg(S, A),
    set_reg(S, C, Va * Vb).

banr(S, A, B, C) ->
    Va = get_reg(S, A),
    Vb = get_reg(S, B),
    set_reg(S, C, Va band Vb).

bani(S, A, Vb, C) ->
    Va = get_reg(S, A),
    set_reg(S, C, Va band Vb).

borr(S, A, B, C) ->
    Va = get_reg(S, A),
    Vb = get_reg(S, B),
    set_reg(S, C, Va bor Vb).

bori(S, A, Vb, C) ->
    Va = get_reg(S, A),
    set_reg(S, C, Va bor Vb).

setr(S, A, _B, C) ->
    Va = get_reg(S, A),
    set_reg(S, C, Va).

seti(S, Va, _B, C) ->
    set_reg(S, C, Va).

gtir(S, Va, B, C) ->
    Vb = get_reg(S, B),
    case Va > Vb of
        true ->
            set_reg(S, C, 1);
        false ->
            set_reg(S, C, 0)
    end.

gtri(S, A, Vb, C) ->
    Va = get_reg(S, A),
    case Va > Vb of
        true ->
            set_reg(S, C, 1);
        false ->
            set_reg(S, C, 0)
    end.

gtrr(S, A, B, C) ->
    Va = get_reg(S, A),
    Vb = get_reg(S, B),
    case Va > Vb of
        true ->
            set_reg(S, C, 1);
        false ->
            set_reg(S, C, 0)
    end.

eqir(S, Va, B, C) ->
    Vb = get_reg(S, B),
    case Va == Vb of
        true ->
            set_reg(S, C, 1);
        false ->
            set_reg(S, C, 0)
    end.

eqri(S, A, Vb, C) ->
    Va = get_reg(S, A),
    case Va == Vb of
        true ->
            set_reg(S, C, 1);
        false ->
            set_reg(S, C, 0)
    end.

eqrr(S, A, B, C) ->
    Va = get_reg(S, A),
    Vb = get_reg(S, B),
    case Va == Vb of
        true ->
            set_reg(S, C, 1);
        false ->
            set_reg(S, C, 0)
    end.
