-module(aoc2020_day21).
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
        problem => {2020, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {IngridiensSets, AllergensSets} = lists:unzip(Data),
    Ingridiens = sets:union(IngridiensSets),
    Alergens = sets:union(AllergensSets),

    IngridientsWAllergen =
        [
            {Allergen, [Is || {Is, As} <- Data, sets:is_element(Allergen, As)]}
         || Allergen <- sets:to_list(Alergens)
        ],

    PossibleAllergens =
        [{Allergen, sets:intersection(Isets)} || {Allergen, Isets} <- IngridientsWAllergen],

    {_Allergens, IngridientsWAllergenSets} = lists:unzip(PossibleAllergens),
    PossibleAlergyIngridients = sets:union(IngridientsWAllergenSets),

    AllergenFree = sets:to_list(sets:subtract(Ingridiens, PossibleAlergyIngridients)),

    length([Af || Af <- AllergenFree, {Is, _As} <- Data, sets:is_element(Af, Is)]).

star2(Data) ->
    {_IngridiensSets, AllergensSets} = lists:unzip(Data),
    Alergens = sets:union(AllergensSets),

    IngridientsWAllergen =
        [
            {Allergen, [Is || {Is, As} <- Data, sets:is_element(Allergen, As)]}
         || Allergen <- sets:to_list(Alergens)
        ],

    PossibleAllergens =
        [{Allergen, sets:intersection(Isets)} || {Allergen, Isets} <- IngridientsWAllergen],

    ProcessResult =
        process_counts([
            {sets:size(Set), Ingridient, Set}
         || {Ingridient, Set} <- PossibleAllergens
        ]),
    {_, ResultList} = lists:unzip(lists:sort(ProcessResult)),
    string:join(ResultList, ",").

read(File) ->
    [process(Line) || Line <- tools:read_lines(File)].

process(Line) ->
    [IngridientsStr, AllergensStr] = string:split(Line, " (contains "),
    Ingridients = string:tokens(IngridientsStr, " "),
    Allergens = string:split(lists:droplast(AllergensStr), ", ", all),

    {sets:from_list(Ingridients), sets:from_list(Allergens)}.

process_counts(List) ->
    process_counts(List, [], []).

process_counts([], Result, []) ->
    Result;
process_counts([], Result, Left) ->
    process_counts(Left, Result, []);
process_counts([{1, Alergen, Set} | Rest], Result, Left) ->
    [Ingridient] = sets:to_list(Set),
    NewResult = [{Alergen, Ingridient} | Result],
    process_counts(remove_ingridient(Rest ++ Left, Ingridient), NewResult, []);
process_counts([First | Rest], Result, Left) ->
    process_counts(Rest, Result, [First | Left]).

remove_ingridient(Lists, Ingridient) ->
    [
        begin
            Set = sets:del_element(Ingridient, OldSet),
            {sets:size(Set), Alergen, Set}
        end
     || {_, Alergen, OldSet} <- Lists
    ].
