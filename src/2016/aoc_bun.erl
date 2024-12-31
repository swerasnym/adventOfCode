-module(aoc_bun).
-export([clear_out/1]).
-export([get_mem/1]).
-export([get_out/1]).
-export([from_lines/1]).
-export([from_file/1]).
-export([run/1]).
-export([set_mem/2]).
-export([set_opt/3]).

-record(bun, {
    cp = 1,
    mem = #{a => 0, b => 0, c => 0, d => 0},
    commands,
    out = [],
    jint = false
}).

from_file(File) ->
    from_lines(tools:read_lines(File)).

from_lines(Lines) ->
    CommandList = [parse_command(string:split(L, " ", all)) || L <- Lines],
    Commands = maps:from_list(lists:enumerate(1, CommandList)),
    #bun{commands = Commands}.

parse_command(["cpy", A, B]) -> {cpy, parse_value(A), parse_value(B)};
parse_command(["dec", A]) -> {dec, parse_value(A)};
parse_command(["inc", A]) -> {inc, parse_value(A)};
parse_command(["jnz", A, B]) -> {jnz, parse_value(A), parse_value(B)};
parse_command(["out", A]) -> {out, parse_value(A)};
parse_command(["tgl", A]) -> {tgl, parse_value(A)}.

parse_value("a") -> a;
parse_value("b") -> b;
parse_value("c") -> c;
parse_value("d") -> d;
parse_value(Integer) -> erlang:list_to_integer(Integer).

get_value(R, #bun{mem = Mem}) when is_atom(R) ->
    maps:get(R, Mem, 0);
get_value(V, _) when is_integer(V) ->
    V.
next(#bun{cp = Cp} = B) ->
    B#bun{cp = Cp + 1}.

next(#bun{cp = Cp, mem = Mem} = B, R, V) when is_atom(R) ->
    B#bun{cp = Cp + 1, mem = Mem#{R => V}}.
jump(B, 0) -> {loop, B};
jump(#bun{cp = Cp} = S, Offset) -> S#bun{cp = Cp + Offset}.

run_command({cpy, X, Y}, B) when is_atom(Y) -> next(B, Y, get_value(X, B));
run_command({inc, X}, B) when is_atom(X) -> next(B, X, get_value(X, B) + 1);
run_command({dec, X}, B) when is_atom(X) -> next(B, X, get_value(X, B) - 1);
run_command({jnz, X, Y}, B) -> run_jnz(X, Y, B);
run_command({out, X}, #bun{out = Out} = B) -> next(B#bun{out = [get_value(X, B) | Out]});
run_command({tgl, X}, B) -> run_tgl(X, B);
run_command(_, B) -> next(B).

run_jnz(X, Y, B) ->
    case get_value(X, B) of
        0 -> next(B);
        _ -> jump(B, get_value(Y, B))
    end.
run_tgl(T, #bun{commands = Cmd, cp = Cp} = B) ->
    Mod = Cp + get_value(T, B),
    case maps:get(Mod, Cmd, none) of
        {inc, X} ->
            next(B#bun{commands = Cmd#{Mod => {dec, X}}});
        {_, X} ->
            next(B#bun{commands = Cmd#{Mod => {inc, X}}});
        {jnz, X, Y} ->
            next(B#bun{commands = Cmd#{Mod => {cpy, X, Y}}});
        {_, X, Y} ->
            next(B#bun{commands = Cmd#{Mod => {jnz, X, Y}}});
        none ->
            next(B)
    end.

run(#bun{cp = Cp, commands = Commands} = B) ->
    case maps:get(Cp, Commands, halt) of
        halt ->
            {halt, B};
        Command when B#bun.jint ->
            run(jint(Command, B));
        Command ->
            run(run_command(Command, B))
    end;
run(Exit) ->
    Exit.

get_mem(#bun{mem = Mem}) -> Mem.
set_mem(#bun{mem = Mem1} = B, Mem2) -> B#bun{mem = maps:merge(Mem1, Mem2)}.
get_out(#bun{out = Out}) -> lists:reverse(Out).
clear_out(#bun{} = B) -> B#bun{out = []}.
set_opt(jint, Value, #bun{} = B) -> B#bun{jint = Value}.

jint(Cmd = {cpy, _, _}, B) ->
    #bun{cp = Cp, commands = Commands} = B,
    case maps:get(Cp + 3, Commands, halt) of
        {jnz, _, -2} ->
            jint_mul(Cmd, B);
        _ ->
            run_command(Cmd, B)
    end;
jint(Cmd, B) ->
    run_command(Cmd, B).

jint_mul(Cmd, B) ->
    #bun{cp = Cp, commands = Commands, mem = Mem} = B,
    Inst = [maps:get(Pos, Commands, halt) || Pos <- lists:seq(Cp, Cp + 5)],
    case Inst of
        [
            {cpy, V, I1},
            {inc, Out},
            {dec, I1},
            {jnz, I1, -2},
            {dec, I2},
            {jnz, I2, -5}
        ] when
            is_atom(I1),
            is_atom(I2),
            is_atom(Out),
            V /= I1,
            V /= I2,
            V /= Out,
            I1 /= I2,
            I1 /= Out,
            I2 /= Out
        ->
            io:format("mul: ~p ~p ~p (~p) ~p*~p~n", [
                V, I2, Out, I1, get_value(V, B), get_value(I2, B)
            ]),
            Res = get_value(V, B) * get_value(I2, B) + get_value(Out, B),
            B#bun{
                cp = Cp + 6,
                mem = Mem#{Out => Res, I1 => 0, I2 => 0}
            };
        [
            {cpy, V, I1},
            {dec, I1},
            {inc, Out},
            {jnz, I1, -2},
            {dec, I2},
            {jnz, I2, -5}
        ] when
            is_atom(I1),
            is_atom(I2),
            is_atom(Out),
            V /= I1,
            V /= I2,
            V /= Out,
            I1 /= I2,
            I1 /= Out,
            I2 /= Out
        ->
            io:format("mul2: ~p ~p ~p (~p) ~p*~p~n", [
                V, I2, Out, I1, get_value(V, B), get_value(I2, B)
            ]),
            Res = get_value(V, B) * get_value(I2, B) + get_value(Out, B),
            B#bun{
                cp = Cp + 6,
                mem = Mem#{Out => Res, I1 => 0, I2 => 0}
            };
        _ ->
            jint_add(Cmd, B)
    end.

jint_add(Cmd, B) ->
    #bun{cp = Cp, commands = Commands, mem = Mem} = B,
    Inst = [maps:get(Pos, Commands, halt) || Pos <- lists:seq(Cp, Cp + 3)],
    case Inst of
        [
            {cpy, V, I1},
            {inc, Out},
            {dec, I1},
            {jnz, I1, -2}
        ] when
            is_atom(I1),
            is_atom(Out),
            V /= I1,
            V /= Out,
            I1 /= Out
        ->
            io:format("add1: ~p ~p (~p)~n", [V, Out, I1]),
            Res = get_value(V, B) + get_value(Out, B),
            B#bun{
                cp = Cp + 4,
                mem = Mem#{Out => Res, I1 => 0}
            };
        [
            {cpy, V, I1},
            {dec, I1},
            {inc, Out},
            {jnz, I1, -2}
        ] when
            is_atom(I1),
            is_atom(Out),
            V /= I1,
            V /= Out,
            I1 /= Out
        ->
            io:format("add2: ~p ~p (~p)~n", [V, Out, I1]),
            Res = get_value(V, B) + get_value(Out, B),
            B#bun{
                cp = Cp + 4,
                mem = Mem#{Out => Res, I1 => 0}
            };
        _ ->
            run_command(Cmd, B)
    end.
