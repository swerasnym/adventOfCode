-module(aoc2023_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day15_ex.txt", star1, 1320},
        {"examples/2023/day15_ex.txt", star2, 145}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Steps) ->
    lists:sum([hash(Step) || Step <- Steps]).

star2(Steps) ->
    Items = [item(Step) || Step <- Steps],
    Groups = maps:groups_from_list(fun hash/1, Items),
    Boxes = [{Group, operate(List)} || Group := List <- Groups],
    FPs = [focus(Box) || Box <- Boxes],
    lists:sum(lists:flatten(FPs)).

read(File) ->
    tools:read_tokens(File, ",").

hash({Label, _}) ->
    hash(Label);
hash(Step) ->
    lists:foldl(fun(C, Acc) -> ((C + Acc) * 17) rem 256 end, 0, Step).

item(Step) ->
    case string:tokens(Step, "-=") of
        [Label] ->
            {Label, minus};
        [Label, FocalLength] ->
            {Label, erlang:list_to_integer(FocalLength)}
    end.

operate(Items) ->
    lists:foldl(fun operation/2, [], Items).

operation({Label, minus}, Acc) ->
    lists:filter(fun({L, _}) -> L /= Label end, Acc);
operation(Item, Acc) ->
    insert(Item, Acc, []).

insert(Item, [], Before) ->
    lists:reverse(Before, [Item]);
insert({Label, _} = Item, [{Label, _} | After], Before) ->
    lists:reverse(Before, [Item | After]);
insert(Item, [H | Rest], Before) ->
    insert(Item, Rest, [H | Before]).

focus({BoxNumber, Items}) ->
    [focus(BoxNumber, EnumeratedItem) || EnumeratedItem <- lists:enumerate(Items)].

focus(Bn, {Slot, {_, FocalLength}}) ->
    (Bn + 1) * Slot * FocalLength.
