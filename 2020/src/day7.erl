-module(day7).

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

star1(Bags) ->
    length(find_all(Bags, ['shiny gold bag', 'dotted violet bag'])).

star2(Bags) ->
    inside(maps:from_list(Bags), 'shiny gold bag').

read(File) ->
    {ok, Bin} = file:read_file(File),
    [process_bag(BagContent)
     || BagContent
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].

process_bag(BagContent) ->
    [Bag, Contents] =
        string:split(
            lists:droplast(BagContent), " contain "),

    {list_to_atom(lists:droplast(Bag)),
     maps:from_list([begin
                         case Content of
                             "no other bags" ->
                                 {emty, 0};
                             _ ->
                                 case string:to_integer(Content) of
                                     {1, Rbag} ->
                                         {list_to_atom(string:trim(Rbag)), 1};
                                     {Number, Rbag} ->
                                         {list_to_atom(lists:droplast(
                                                           string:trim(Rbag))),
                                          Number}
                                 end
                         end
                     end
                     || Content <- string:split(Contents, ", ", all)])}.

find_all(Bags, Keys) ->
    F = fun(Bag) -> has_any(Bag, Keys) end,

    case lists:partition(F, Bags) of
        {[], _NotSatisfying} ->
            [];
        {Satisfying, NotSatisfying} ->
            Satisfying ++ find_all(NotSatisfying, [A || {A, _} <- Satisfying])
    end.

has_any(_Bag, []) ->
    false;
has_any({Bag, Map}, [Key | Keys]) ->
    case maps:is_key(Key, Map) of
        true ->
            true;
        false ->
            has_any({Bag, Map}, Keys)
    end.

inside(_Map, emty) ->
    0;
inside(Map, Bag) ->
    Content =
        maps:to_list(
            maps:get(Bag, Map)),
    lists:sum([(1 + inside(Map, IBag)) * N || {IBag, N} <- Content]).
