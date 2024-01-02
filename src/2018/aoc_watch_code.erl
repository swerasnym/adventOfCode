-module(aoc_watch_code).
-compile({inline, [execute/1, get_reg/2, set_reg/3]}).

%% Defined to take 3 parameters (in addition to state) in problem description.
-hank([{unnecessary_function_arguments, [{seti, 4}]}]).
-hank([{unnecessary_function_arguments, [{setr, 4}]}]).

-export([new/0]).
-export([new/1]).
-export([set_registers/2]).
-export([get_registers/1]).
-export([instructions_map/0]).
-export([set_ip_reg/2]).
-export([load_program/2]).
-export([run/1]).
-export([set_reg/3]).
-export([get_reg/2]).
-export([run_dbg/2]).
-export([run_dbg2/3]).

-record(state, {reg, reg_size, ip_reg = 5, program = #{}}).

new() ->
    new(6).

new(List) when is_list(List) ->
    set_registers(new(length(List)), List);
new(N) ->
    set_reg(#state{reg = atomics:new(N + 1, [{signed, true}]), reg_size = N, ip_reg = N}, N, -1).

get_reg(#state{reg = Reg}, I) ->
    atomics:get(Reg, I + 1).

set_reg(#state{reg = Reg} = S, I, V) ->
    atomics:put(Reg, I + 1, V),
    S.

set_registers(#state{reg_size = N} = S, Values) when length(Values) == N ->
    [set_reg(S, I, V) || {I, V} <- lists:enumerate(0, Values)],
    S.

get_registers(#state{reg_size = N} = S) ->
    [get_reg(S, I) || I <- lists:seq(0, N - 1)].

set_ip_reg(S, IpReg) ->
    set_reg(S#state{ip_reg = IpReg}, IpReg, -1).

load_program(S, Program) when is_list(Program) ->
    Pgm = [[{I, compile(S, Line)} | Param] || [I | Param] = Line <- Program],
    S#state{program = maps:from_list(lists:enumerate(0, Pgm))}.

run(#state{reg = Reg, ip_reg = IpReg, program = Program} = S) ->
    Line = atomics:add_get(Reg, IpReg + 1, 1),
    case maps:get(Line, Program, halt) of
        halt ->
            S;
        Instructions ->
            execute(Instructions),
            run(S)
    end.

run_dbg(S, 0) ->
    io:format("Break!~n"),
    S;
run_dbg(#state{reg = Reg, ip_reg = IpReg, program = Program} = S, N) ->
    Line = atomics:add_get(Reg, IpReg + 1, 1),
    case maps:get(Line, Program, halt) of
        halt ->
            S;
        [{I, _} | Rest] = Instructions ->
            execute(Instructions),
            io:format("IP: ~p\t Inst: ~0p \t Regs: ~0p ~n", [Line, [I | Rest], get_registers(S)]),
            run_dbg(S, N - 1)
    end.

run_dbg2(S, 0, _) ->
    io:format("Break!~n"),
    S;
run_dbg2(#state{reg = Reg, ip_reg = IpReg, program = Program} = S, N, {DbgLine, Regs} = DbgPts) ->
    Line = atomics:add_get(Reg, IpReg + 1, 1),
    case maps:get(Line, Program, halt) of
        halt ->
            S;
        Instructions ->
            execute(Instructions),
            case Line == DbgLine of
                true ->
                    Key = [get_reg(S, I) || I <- Regs],
                    case erlang:get(Key) of
                        undefined ->
                            erlang:put(Key, N),
                            % io:format("N: ~p\tIP: ~p\t Inst: ~0p \t Regs: ~0p ~n", [
                            %     N, Line, Instructions, get_registers(S)
                            % ]),
                            run_dbg2(S, N - 1, DbgPts);
                        _ ->
                            S
                    end;
                false ->
                    run_dbg2(S, N, DbgPts)
            end
    end.

execute([{_I, Fun} | _]) ->
    Fun().

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
compile(S, [divi, A, B, C]) ->
    fun() -> divi(S, A, B, C) end;
compile(#state{reg = Reg}, [addr, A, B, A]) ->
    fun() -> atomics:add(Reg, A + 1, atomics:get(Reg, B + 1)) end;
compile(#state{reg = Reg}, [addr, A, B, B]) ->
    fun() -> atomics:add(Reg, B + 1, atomics:get(Reg, A + 1)) end;
compile(#state{reg = Reg}, [addi, A, Vb, A]) ->
    fun() -> atomics:add(Reg, A + 1, Vb) end;
compile(#state{reg = Reg}, [setr, A, _, C]) ->
    fun() -> atomics:put(Reg, C + 1, atomics:get(Reg, A + 1)) end;
compile(#state{reg = Reg}, [seti, Va, _, C]) ->
    fun() -> atomics:put(Reg, C + 1, Va) end;
compile(S, [I, A, B, C]) ->
    Fun = maps:get(I, instructions_map()),
    fun() -> Fun(S, A, B, C) end.

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

setr(#state{reg = Reg}, A, _B, C) ->
    atomics:put(Reg, C + 1, atomics:get(Reg, A + 1)).

seti(#state{reg = Reg}, Va, _B, C) ->
    atomics:put(Reg, C + 1, Va).

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

%% Extra instruction to speed up day 21...
divi(S, A, Vb, C) ->
    Va = get_reg(S, A),
    set_reg(S, C, Va div Vb).
