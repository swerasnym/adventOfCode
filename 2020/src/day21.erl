-module(day21).

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

star1(Data) ->
    {IngridiensSets, AllergensSets} = lists:unzip(Data),
    Ingridiens = sets:union(IngridiensSets),
    Alergens = sets:union(AllergensSets),

    IngridientsWAllergen =
        [{Allergen, [Is || {Is, As} <- Data, sets:is_element(Allergen, As)]}
         || Allergen <- sets:to_list(Alergens)],

    PossibleAllergens =
        [{Allergen, sets:intersection(Isets)} || {Allergen, Isets} <- IngridientsWAllergen],

    {_Allergens, IngridientsWAllergenSets} = lists:unzip(PossibleAllergens),
    PossibleAlergyIngridients = sets:union(IngridientsWAllergenSets),

    AllergenFree =
        sets:to_list(
            sets:subtract(Ingridiens, PossibleAlergyIngridients)),

    length([Af || Af <- AllergenFree, {Is, _As} <- Data, sets:is_element(Af, Is)]).

star2(Data) ->
    {_IngridiensSets, AllergensSets} = lists:unzip(Data),
    Alergens = sets:union(AllergensSets),

    IngridientsWAllergen =
        [{Allergen, [Is || {Is, As} <- Data, sets:is_element(Allergen, As)]}
         || Allergen <- sets:to_list(Alergens)],

    PossibleAllergens =
        [{Allergen, sets:intersection(Isets)} || {Allergen, Isets} <- IngridientsWAllergen],

    ProcessResult =
        process_counts([{sets:size(Set), Ingridient, Set}
                        || {Ingridient, Set} <- PossibleAllergens]),
    {_, ResultList} =
        lists:unzip(
            lists:sort(ProcessResult)),
    string:join(ResultList, ",").

read(File) ->
    [process(Line) || Line <- tools:read_lines(File)].

process(Line) ->
    [IngridientsStr, AllergensStr] = string:split(Line, " (contains "),
    Ingridients = string:tokens(IngridientsStr, " "),
    Allergens =
        string:split(
            lists:droplast(AllergensStr), ", ", all),

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
    [begin
         Set = sets:del_element(Ingridient, OldSet),
         {sets:size(Set), Alergen, Set}
     end
     || {_, Alergen, OldSet} <- Lists].
