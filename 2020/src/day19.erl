-module(day19).

-export([run/2]).

run(Star, File) ->
    Data = read(File),

    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

star1({RuleList, Messages}) ->
    Map = maps:from_list(RuleList),
    Strings = maps:from_list([{V, ok} || V <- build(0, Map)]),
    #{ok:=Result} = count([maps:get(M, Strings, nok) || M <- Messages ]),
    Result.
    
-define(LEN, 8).
star2({RuleList, Messages}) ->
    %% 8: 42 | 42 8
    %% 11: 42 31 | 42 11 31
    %% observation rules 8, 11 only called by rule zero as 0: 8 11
    %% All substrings by rules 42, 31 have lenght 8.
    %% All messages length 24..88

    Map = maps:from_list(RuleList),
    Map42 = maps:from_list([{V, ok} || V <- build(42, Map)]),
    Map31 = maps:from_list([{V, ok} || V <- build(31, Map)]),
    #{ok:=Result} = count([ matches_substr(M, Map42, Map31,0) || M <- Messages ]),
    Result.


matches_substr([], _Map42, _Map31, _N) ->
    nok;
matches_substr(String, Map42, Map31, N) ->
    {Head, Tail} = lists:split(?LEN, String),
    Result = 
        case maps:get(Head, Map42, nok) of
            ok ->
                matches_substr(Tail, Map42, Map31, N+1);
            nok ->
                matches_substr(String, Map31, N)
        end,
    Result.



matches_substr([],  _Map31, N)  when N > 0->
    ok;
matches_substr(_,  _Map31,0) ->
    nok;
matches_substr(String,  Map31, N) ->
    {Head, Tail} = lists:split(?LEN, String),
    Result =
        case maps:get( Head, Map31 ,nok) of
            ok ->
                matches_substr(Tail,  Map31, N-1);
            nok ->
                nok
        end,

    Result.


read(File) ->
    {ok, Bin} = file:read_file(File),

    
    [Rules, Messages] = string:split(string:trim(binary_to_list(Bin)), "\n\n", all),
    {[parse_rule(Rule) || Rule <- string:split(Rules, "\n", all) ] ,string:split(Messages, "\n", all)}.
    


count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).

parse_rule(Line) ->
    {Id, ": "++Rule} = string:to_integer(Line),
    case Rule of 
        "\""++ Rest ->
            {Id, {str, lists:droplast(Rest)}};
        RuleLists ->
            Rules = string:split(RuleLists, " | ", all),
            {Id, {rule, [[list_to_integer(R) || R<- string:split(Ru, " ", all)] || Ru <- Rules ]}}
end.
            


build(Id, Map) -> 
    case maps:get(Id, Map) of 
        {str, Str} ->
            [Str];
        {rule, Rules} ->
                     
                     lists:flatmap(fun (Ids) -> build_rule(Ids, Map) end, Rules)                     
             end.
    

build_rule(Ids, Map) ->
    build_rule(Ids, [""], Map ).


build_rule([], Results, _Map)->
    Results;
build_rule([Id|Ids], Results, Map) ->
    Recurse = build(Id, Map),
    build_rule(Ids, [A++B || A <- Results, B <- Recurse], Map).


    


