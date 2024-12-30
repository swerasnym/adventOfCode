-module(aoc_tablet).
-export([clear_output/1]).
-export([get_mem/1]).
-export([get_output/1]).
-export([from_lines/1]).
-export([from_file/1]).
-export([run/1]).
-export([set_mem/2]).
-export([add_input/2]).
-export([debug/2]).
-export([get_calls/1]).

-record(s, {
    cp = 1,
    mem = #{},
    commands,
    out = [],
    in = [],
    debug = false,
    calls = #{}
}).

from_file(File) ->
    from_lines(tools:read_lines(File)).

from_lines(Lines) ->
    CommandList = [parse_command(string:split(L, " ", all)) || L <- Lines],
    Commands = maps:from_list(lists:enumerate(1, CommandList)),
    #s{commands = Commands}.

parse_command(["rcv", X]) -> {rcv, parse_value(X)};
parse_command(["snd", X]) -> {snd, parse_value(X)};
parse_command(["add", X, Y]) -> {add, parse_value(X), parse_value(Y)};
parse_command(["jgz", X, Y]) -> {jgz, parse_value(X), parse_value(Y)};
parse_command(["jnz", X, Y]) -> {jnz, parse_value(X), parse_value(Y)};
parse_command(["mod", X, Y]) -> {mod, parse_value(X), parse_value(Y)};
parse_command(["mul", X, Y]) -> {mul, parse_value(X), parse_value(Y)};
parse_command(["set", X, Y]) -> {set, parse_value(X), parse_value(Y)};
parse_command(["sub", X, Y]) -> {sub, parse_value(X), parse_value(Y)}.

parse_value([L]) when L >= $a, L =< $z -> list_to_atom([L]);
parse_value(Integer) -> erlang:list_to_integer(Integer).

get_v(R, #s{mem = Mem}) when is_atom(R) -> maps:get(R, Mem, 0);
get_v(V, _) when is_integer(V) -> V.

next(#s{cp = Cp} = S) -> S#s{cp = Cp + 1}.

next_out(#s{cp = Cp, mem = Mem} = S, R, V) when is_atom(R) ->
    S#s{cp = Cp + 1, mem = Mem#{R => V}};
next_out(S, _, _) ->
    {invalid_write, S}.

jump(S, 0) -> {loop, S};
jump(#s{cp = Cp} = S, Offset) -> S#s{cp = Cp + Offset}.
run_command({set, X, Y}, S) -> next_out(S, X, get_v(Y, S));
run_command({add, X, Y}, S) -> next_out(S, X, get_v(X, S) + get_v(Y, S));
run_command({sub, X, Y}, S) -> next_out(S, X, get_v(X, S) - get_v(Y, S));
run_command({mul, X, Y}, S) -> next_out(S, X, get_v(X, S) * get_v(Y, S));
run_command({mod, X, Y}, S) -> next_out(S, X, tools:mod(get_v(X, S), get_v(Y, S)));
run_command({jgz, X, Y}, S) -> run_jgz(X, Y, S);
run_command({jnz, X, Y}, S) -> run_jnz(X, Y, S);
run_command({rcv, X}, S) -> run_rcv(X, S);
run_command({snd, X}, S) -> run_snd(X, S);
run_command(_, S) -> {illegal_instruction, S}.

run_rcv(X, #s{in = In} = S) ->
    case In of
        [] -> {rcv, S};
        [V | Rest] -> next_out(S#s{in = Rest}, X, V)
    end.

run_snd(X, #s{out = Out} = S) ->
    next(S#s{out = [get_v(X, S) | Out]}).

run_jgz(X, Y, S) ->
    case get_v(X, S) > 0 of
        false -> next(S);
        true -> jump(S, get_v(Y, S))
    end.
run_jnz(X, Y, S) ->
    case get_v(X, S) of
        0 -> next(S);
        _ -> jump(S, get_v(Y, S))
    end.

run(#s{cp = Cp, commands = Commands} = S) ->
    case maps:get(Cp, Commands, halt) of
        halt ->
            {halt, S};
        Command when S#s.debug ->
            run(run_command(Command, log_call(Command, S)));
        Command ->
            run(run_command(Command, S))
    end;
run(Exit) ->
    Exit.

get_mem(#s{mem = Mem}) -> Mem.
set_mem(#s{mem = Mem1} = S, Mem2) -> S#s{mem = maps:merge(Mem1, Mem2)}.

add_input(#s{in = In} = S, L) -> S#s{in = In ++ L}.
get_output(#s{out = Out}) -> lists:reverse(Out).
clear_output(#s{} = S) -> S#s{out = []}.
debug(#s{} = S, Debug) when is_boolean(Debug) -> S#s{debug = Debug}.
get_calls(#s{calls = Calls}) -> Calls.

log_call(Command, #s{calls = Calls} = S) ->
    Call = erlang:element(1, Command),
    V = maps:get(Call, Calls, 0),
    S#s{calls = Calls#{Call => V + 1}}.
