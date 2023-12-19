-module(aoc2023_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day19_ex.txt", star1, 19114},
        {"2023/data/day19_ex.txt", star2, 167409079868000}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 19},
        examples => Examples
    }).

run() ->
    %, all, examples).
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Workflows, Ratings}) ->
    Check = [check_rating("in", Workflows, Rating) || Rating <- Ratings],

    lists:sum([Sum || {accept, #{sum := Sum}} <- Check]).

star2({Workflows, _}) ->
    I = {1, 4000},
    Rating = #{"x" => I, "m" => I, "a" => I, "s" => I},
    Accepted = check_rating2([{"in", Rating}], Workflows),
    %  io:format("~p~n", [Accepted]),

    lists:sum([sum(A) || A <- Accepted]).

read(File) ->
    [W, R] = tools:read_blocks(File),
    WFs = tools:parse_lines(W, fun workflow/1),

    {maps:from_list(WFs), ratings(R)}.

check_rating(WF, Workflows, Rating) ->
    Rules = maps:get(WF, Workflows),
    case check(Rules, Rating) of
        accept ->
            {accept, Rating};
        reject ->
            {reject, Rating};
        Next ->
            check_rating(Next, Workflows, Rating)
    end.

check([{Part, "<", Val, True} | Rest], Rating) ->
    case maps:get(Part, Rating) < Val of
        true ->
            True;
        false ->
            check(Rest, Rating)
    end;
check([{Part, ">", Val, True} | Rest], Rating) ->
    case maps:get(Part, Rating) > Val of
        true ->
            True;
        false ->
            check(Rest, Rating)
    end;
check([Else], #{}) ->
    Else.
check_rating2([], _) ->
    [];
check_rating2(WFs, Workflows) ->
    %  io:format("~p~n", [WFs]),

    Lists = [check2(maps:get(WF, Workflows), Rating) || {WF, Rating} <- WFs],

    List = lists:flatten(Lists),
    Accepted = [A || {accept, A} <- List],
    Next = [{WF, N} || {WF, N} <- List, WF /= reject, WF /= accept],

    Accepted ++ check_rating2(Next, Workflows).

check2([{Part, "<", Val, True} | Rest], Rating) ->
    Intervall = maps:get(Part, Rating),
    Before = tools:interval_before(Intervall, {Val - 1, Val - 1}),
    After = tools:interval_after(Intervall, {Val, Val}),
    case {Before, After} of
        {empty, _} ->
            io:format("~p ~p~n", [{Part, "<", Val, True}, Rating]);
        {_, empty} ->
            io:format("~p ~p~n", [{Part, "<", Val, True}, Rating]);
        _ ->
            ok
    end,
    [{True, Rating#{Part := Before}} | check2(Rest, Rating#{Part := After})];
check2([{Part, ">", Val, True} | Rest], Rating) ->
    Intervall = maps:get(Part, Rating),
    Before = tools:interval_before(Intervall, {Val, Val}),
    After = tools:interval_after(Intervall, {Val + 1, Val + 1}),

    case {Before, After} of
        {empty, _} ->
            io:format("~p ~p~n", [{Part, ">", Val, True}, Rating]);
        {_, empty} ->
            io:format("~p ~p~n", [{Part, ">", Val, True}, Rating]);
        _ ->
            ok
    end,
    [{True, Rating#{Part := After}} | check2(Rest, Rating#{Part := Before})];
check2([Else], Rating) ->
    [{Else, Rating}].

workflow(W) ->
    [Name, RulesS] = string:tokens(W, "{}"),
    Rules = string:tokens(RulesS, ","),
    {Name, rule(Rules, [])}.

rule([R], Acc) ->
    lists:reverse(Acc, [accept_reject(R)]);
rule([R | Rest], Acc) ->
    [[Part, Oper, Val, WF]] = tools:parse_format(R, "~c~c~d:~s"),

    rule(Rest, [{Part, Oper, Val, accept_reject(WF)} | Acc]).

accept_reject("A") ->
    accept;
accept_reject("R") ->
    reject;
accept_reject(V) ->
    V.
ratings(R) ->
    [
        #{"x" => X, "m" => M, "a" => A, "s" => S, sum => X + M + A + S}
     || [X, M, A, S] <- tools:parse_format(R, "{x=~d,m=~d,a=~d,s=~d}\n")
    ].

sum(Map) ->
    tools:product([length_i(V) || V <- maps:values(Map)]).

length_i(empty) ->
    1;
length_i({A, B}) ->
    B - A + 1.
% Lines = tools:parse_lines(R),
    % [tools:parse_format(L, "{x=~d,m=~d,a=~d,s=~d}") || L <- Lines].
