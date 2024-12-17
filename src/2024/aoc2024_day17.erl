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
    PMap = maps:from_list(lists:enumerate(0, Program)),
    Res = execute(State, PMap),
    string:join([integer_to_list(I) || I <- Res], ",").

star2({State, Program}) ->
    PMap = maps:from_list(lists:enumerate(0, Program)),
    Res = solve_a([0], lists:reverse(Program), [], PMap),
    % Sanity check...
    Program = execute(State#state{a = Res}, PMap),
    Res.

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
instruction(0, Op, S = #state{a = A}) ->
    S#state{a = A bsr cop(Op, S)};
instruction(1, Op, S = #state{b = B}) ->
    S#state{b = B bxor Op};
instruction(2, Op, S = #state{}) ->
    S#state{b = tools:mod(cop(Op, S), 8)};
instruction(3, _Op, S = #state{a = 0}) ->
    S;
instruction(4, _Op, S = #state{b = B, c = C}) ->
    S#state{b = B bxor C};
instruction(3, Op, S = #state{}) ->
    S#state{ip = Op - 2};
instruction(5, Op, S = #state{output = O}) ->
    S#state{output = [tools:mod(cop(Op, S), 8) | O]};
instruction(6, Op, S = #state{a = A}) ->
    S#state{b = A bsr cop(Op, S)};
instruction(7, Op, S = #state{a = A}) ->
    S#state{c = A bsr cop(Op, S)}.

execute(S = #state{ip = Ip, output = Out}, Program) ->
    case {maps:get(Ip, Program, exit), maps:get(Ip + 1, Program, exit)} of
        {exit, exit} ->
            lists:reverse(Out);
        {Opt, Oper} ->
            S0 = instruction(Opt, Oper, S),
            execute(S0#state{ip = S0#state.ip + 2}, Program)
    end.

% Assume that we can generate A 3 bits at a time and that B & C is generated from A each printout.
solve_a(As, [], _, _) ->
    io:format("~p~n", [As]),
    lists:min(As);
solve_a(As, [Next | Rest], Tail, PMap) ->
    Expected = [Next | Tail],
    NewAs = [
        A * 8 + B
     || A <- As, B <- lists:seq(0, 7), Expected == execute(#state{a = A * 8 + B}, PMap)
    ],
    solve_a(NewAs, Rest, Expected, PMap).
