-module(aoc2020_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 19},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({RuleMap, Messages}) ->
    %% Observation:
    %% 0: 8 11
    %% 8: 42
    %% 11: 42 31
    %% Gives:
    %% 0: 42 42 31
    Rule42 = build(42, RuleMap),
    Rule31 = build(31, RuleMap),
    length([Message || Message <- Messages, validate1(Message, [Rule42, Rule42, Rule31])]).

star2({RuleMap, Messages}) ->
    %% Observation:
    %% 0: 8 11
    %% 8: 42 | 42 8
    %% 11: 42 31 | 42 11 31
    %% Gives
    %% 0: 42{m} 42{n} 31{n}
    %% Where m,n /= 0.
    Rule42 = build(42, RuleMap),
    Rule31 = build(31, RuleMap),
    length([Message || Message <- Messages, validate2(Message, Rule42, Rule31, 0)]).

read(File) ->
    [Rules, Messages] = tools:read_blocks(File),
    {
        maps:from_list([parse_rule(RuleLine) || RuleLine <- tools:parse_lines(Rules)]),
        tools:parse_lines(Messages)
    }.

parse_rule(RuleLine) ->
    {Id, ": " ++ RuleList} = string:to_integer(RuleLine),
    case RuleList of
        "\"" ++ Rest ->
            {Id, {str, lists:droplast(Rest)}};
        RuleLists ->
            % eqwalizer:ignore
            Rules = string:split(RuleLists, " | ", all),
            {Id, {rule, [tools:parse_integers(Rule) || Rule <- Rules]}}
    end.

build(Id, Map) ->
    case maps:get(Id, Map) of
        {str, Str} ->
            [Str];
        {rule, Rules} ->
            lists:flatmap(fun(Ids) -> build_rule(Ids, Map) end, Rules)
    end.

build_rule(Ids, Map) ->
    build_rule(Ids, [""], Map).

build_rule([], Results, _Map) ->
    Results;
build_rule([Id | Ids], Results, Map) ->
    build_rule(Ids, [A ++ B || A <- Results, B <- build(Id, Map)], Map).

remove_prefix(String, Prefixes) ->
    case lists:search(fun(Prefix) -> lists:prefix(Prefix, String) end, Prefixes) of
        {value, Prefix} ->
            lists:nthtail(length(Prefix), String);
        false ->
            false
    end.

validate1([], []) ->
    true;
validate1(_, []) ->
    false;
validate1(false, _) ->
    false;
validate1(String, [Prefix | Rest]) ->
    validate1(remove_prefix(String, Prefix), Rest).

validate2([], _Rule31, N) ->
    N > 0;
validate2(_, [], _N) ->
    false;
validate2(String, Rule31, N) ->
    case remove_prefix(String, Rule31) of
        false ->
            false;
        NewString ->
            validate2(NewString, Rule31, N - 1)
    end.

validate2([], _Rule42, _Rule31, _N) ->
    false;
validate2(String, Rule42, Rule31, N) ->
    case remove_prefix(String, Rule42) of
        false ->
            validate2(String, Rule31, N);
        NewString ->
            validate2(NewString, Rule42, Rule31, N + 1)
    end.
