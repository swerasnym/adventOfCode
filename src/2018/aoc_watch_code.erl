-module(aoc_watch_code).

%% Defined to take 3 parameters (in addition to state) in problem description.
-hank([{unnecessary_function_arguments, [{seti, 4}]}]).
-hank([{unnecessary_function_arguments, [{setr, 4}]}]).

-export([new/0]).
-export([new/1]).
-export([set_registers/2]).
-export([get_registers/1]).
-export([instructions_map/0]).
-export([execute/2]).

-record(state, {reg, reg_size}).

new() ->
    new(4).

new(List) when is_list(List) ->
    set_registers(new(length(List)), List);
new(N) ->
    #state{reg = array:new(N, [{fixed, true}, {default, 0}]), reg_size = N}.

set_registers(#state{reg_size = N} = S, Values) when length(Values) == N ->
    S#state{reg = array:fix(array:from_list(Values))}.

get_registers(#state{reg = Reg}) ->
    array:to_list(Reg).

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
