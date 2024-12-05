-module(aoc2024_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day5_ex.txt", star1, 143},
        {"examples/2024/day5_ex.txt", star2, 123}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Rules, Pages}) ->
    lists:sum([middle(L) || L <- Pages, check_rules(Rules, L)]).

star2({Rules, Pages}) ->
    Reordered = [reorder(Rules, L) || L <- Pages, not check_rules(Rules, L)],
    lists:sum([middle(R) || R <- Reordered]).

read(File) ->
    [Rules, Pages] = tools:read_blocks(File),
    {
        tools:parse_lines(Rules, fun(L) -> tools:parse_integers(L, "|") end),
        tools:parse_lines(Pages, fun(L) -> tools:parse_integers(L, ",") end)
    }.

check_rules(Rules, List) ->
    lists:all(fun(Rule) -> in_order(Rule, List) end, Rules).

in_order(_, []) ->
    true;
in_order([A, _], [A | _]) ->
    true;
in_order([A, B], [B | List]) ->
    not lists:member(A, List);
in_order(Rule, [_ | Rest]) ->
    in_order(Rule, Rest).

middle(List) ->
    MiddleIdx = length(List) div 2 + 1,
    lists:nth(MiddleIdx, List).

reorder(Rules, Pages) ->
    G = digraph:new(),
    [
        digraph:add_edge(G, digraph:add_vertex(G, A), digraph:add_vertex(G, B))
     || [A, B] <- Rules, lists:member(A, Pages), lists:member(B, Pages)
    ],
    digraph_utils:topsort(G).
