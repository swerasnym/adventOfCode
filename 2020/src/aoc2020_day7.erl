-module(aoc2020_day7).
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
        problem => {2020, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Bags) ->
    length(find_all(Bags, ['shiny gold bag'])).

star2(Bags) ->
    inside(maps:from_list(Bags), 'shiny gold bag').

read(File) ->
    [process_bag(BagContent) || BagContent <- tools:read_lines(File)].

process_bag(BagContent) ->
    [Bag, Contents] = string:split(lists:droplast(BagContent), "s contain "),

    F = fun
        ("no other bags") ->
            {emty, 0};
        (Content) ->
            case string:to_integer(Content) of
                {1, " " ++ Rbag} ->
                    {list_to_atom(Rbag), 1};
                {Number, " " ++ Rbag} ->
                    {list_to_atom(lists:droplast(Rbag)), Number}
            end
    end,
    {
        list_to_atom(Bag),
        maps:from_list([F(Content) || Content <- string:split(Contents, ", ", all)])
    }.

find_all(Bags, Keys) ->
    F = fun({_Bag, Map}) -> has_any(Map, Keys) end,

    case lists:partition(F, Bags) of
        {[], _NotSatisfying} ->
            [];
        {Satisfying, NotSatisfying} ->
            Satisfying ++ find_all(NotSatisfying, [A || {A, _} <- Satisfying])
    end.

has_any(_Bag, []) ->
    false;
has_any(Map, [Key | Keys]) ->
    case maps:is_key(Key, Map) of
        true ->
            true;
        false ->
            has_any(Map, Keys)
    end.

inside(_Map, emty) ->
    0;
inside(Map, Bag) ->
    Content = maps:to_list(maps:get(Bag, Map)),
    lists:sum([(1 + inside(Map, IBag)) * N || {IBag, N} <- Content]).
