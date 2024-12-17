-module(aoc2024_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day17_ex.txt", star1, "4,6,3,5,6,3,5,2,1,0"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 17},
        examples => Examples
    }).

-record(state, {a, b, c, ip = 0, output = []}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({State, Program}) ->
    io:format("~p~n", [State]),
    Pmap = maps:from_list(lists:enumerate(0, Program)),
    Res = execute(State, Pmap),
    string:join([integer_to_list(I) || I <- Res], ",").

star2({State, Program}) ->
    Pmap = maps:from_list(lists:enumerate(0, Program)),
    Res = solve(0, lists:reverse(Program)),
    Res = search(Res, State, Program, Pmap).

read(File) ->
    [Regs, "Program: " ++ ProgramS] = tools:read_blocks(File),
    [A, B, C] = tools:parse_format(Regs, "Register A: ~d\nRegister B: ~d\nRegister C: ~d"),
    Program = tools:parse_integers(ProgramS, ","),
    {#state{a = A, b = B, c = C}, Program}.

cop(4, #state{a = A}) ->
    A;
cop(5, #state{b = B}) ->
    B;
cop(6, #state{c = C}) ->
    C;
cop(N, _) when N < 4 andalso N >= 0 ->
    N.

%% Opt, State
optcode(0, Op, S = #state{a = A}) ->
    Cop = cop(Op, S),
    S#state{a = A div tools:pow(2, Cop)};
optcode(1, Op, S = #state{b = B}) ->
    S#state{b = B bxor Op};
optcode(2, Op, S = #state{}) ->
    S#state{b = tools:mod(cop(Op, S), 8)};
optcode(3, _Op, S = #state{a = 0}) ->
    S;
optcode(3, Op, S = #state{}) ->
    S#state{ip = Op - 2};
optcode(4, _Op, S = #state{b = B, c = C}) ->
    S#state{b = B bxor C};
optcode(5, Op, S = #state{output = O}) ->
    S#state{output = [tools:mod(cop(Op, S), 8) | O]};
optcode(6, Op, S = #state{a = A}) ->
    Cop = cop(Op, S),
    S#state{b = A div tools:pow(2, Cop)};
optcode(7, Op, S = #state{a = A}) ->
    Cop = cop(Op, S),
    S#state{c = A div tools:pow(2, Cop)}.

execute(S = #state{ip = Ip, output = Out}, Program) ->
    case {maps:get(Ip, Program, exit), maps:get(Ip + 1, Program, exit)} of
        {exit, exit} ->
            lists:reverse(Out);
        {Opt, Oper} ->
            S0 = optcode(Opt, Oper, S),
            %  io:format("Ip~p: ~p ~p -> ~p~n", [Ip, Opt, Oper, S0]),
            execute(S0#state{ip = S0#state.ip + 2}, Program)
    end.

search(N, S, Program, Pmap) ->
    case execute(S#state{a = N}, Pmap) of
        Program ->
            N;
        P when length(P) == length(Program) ->
            search(N + 1, S, Program, Pmap)
    end.

solve(A, []) ->
    A;
solve(A, [P | Rest]) ->
    Apos = [(A * 8) bor Bc || Bc <- lists:seq(0, 7), P == bs(A, Bc)],
    As = [solve(Ax, Rest) || Ax <- Apos, none /= solve(Ax, Rest)],
    case As of
        [] ->
            none;
        Result ->
            lists:min(Result)
    end.

%% Tuned to my input...
bs(A, Bc) ->
    Ta = (A bsl 3) bor Bc,
    Bc1 = Bc bxor 1,
    C = (Ta bsr Bc1),
    Bc2 = Bc1 bxor 4,
    Bc3 = Bc2 bxor C,
    Bc3 band 7.
