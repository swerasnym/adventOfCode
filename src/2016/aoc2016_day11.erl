-module(aoc2016_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day11_ex.txt", star1, 11}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 11},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Floors) ->
    {Steps, _, _} = aoc_graph:a_star({1, Floors}, fun is_end/1, fun neighbours/1, fun estimate/1),
    Steps.

star2(Floors) ->
    New = [
        {elerium, generator}, {elerium, microchip}, {dilithium, generator}, {dilithium, microchip}
    ],
    Items1 = lists:sort(get_floor(1, Floors) ++ New),
    Floors1 = erlang:setelement(1, Floors, lists:sort(Items1)),
    {Steps, _, _} = aoc_graph:a_star({1, Floors1}, fun is_end/1, fun neighbours/1, fun estimate/1),
    Steps.

read(File) ->
    erlang:list_to_tuple(tools:read_lines(File, fun parse_floor/1)).

parse_floor(Text) ->
    NoPeriod = lists:droplast(Text),
    [_, InventoryAAnd0] = string:split(NoPeriod, "contains "),
    case InventoryAAnd0 of
        "nothing relevant" ->
            [];
        "a " ++ InventoryAAnd ->
            InventoryA = string:replace(InventoryAAnd, " and ", " "),
            Inventory = string:replace(InventoryA, ",", "", all),
            lists:sort([parse_item(I) || I <- string:split(lists:flatten(Inventory), " a ", all)])
    end.

parse_item(Item0) ->
    Item = string:replace(Item0, "-compatible", "", all),
    erlang:list_to_tuple(tools:parse_format(lists:flatten(Item), "~a ~a")).

is_end({4, {[], [], [], _}}) -> true;
is_end({_, {_, _, _, _}}) -> false.

estimate({_, {A, B, C, _}}) ->
    (length(A) * 3 + length(B) * 2 + length(C)) div 2.

is_safe(Floor) ->
    lists:all(fun(I) -> is_safe(I, Floor) end, Floor).

is_safe({_, generator}, _) ->
    true;
is_safe({T, microchip}, Floor) ->
    Generators = [G || {G, generator} <- Floor],
    Generators == [] orelse lists:member(T, Generators).

neighbours({F, Floors} = S) ->
    Items = select_items(get_floor(F, Floors)),
    Tos = select_floors(F),

    NewFloors = lists:flatten([move(To, I, S) || To <- Tos, I <- Items]),
    {Up, Down} = lists:partition(fun({_, {To, _}}) -> To > F end, NewFloors),
    [{1, Next} || Next <- filter_up(Up) ++ filter_down(Down)].

move(To, Items, {From, Floors}) ->
    New = lists:sort(get_floor(To, Floors) ++ Items),
    Old = lists:sort(get_floor(From, Floors) -- Items),
    case is_safe(New) andalso is_safe(Old) of
        false ->
            [];
        true ->
            Res0 = erlang:setelement(To, Floors, New),
            {length(Items), {To, compress(erlang:setelement(From, Res0, Old))}}
    end.

filter_down(L) ->
    case [S || {1, S} <- L] of
        [] ->
            [S || {2, S} <- L];
        One ->
            One
    end.

filter_up(L) ->
    case [S || {2, S} <- L] of
        [] ->
            [S || {1, S} <- L];
        Two ->
            Two
    end.

compress({A, B, C, D}) ->
    Microchips = [M || {M, microchip} <- A ++ B ++ C ++ D],
    Map = #{M => N || {N, M} <- lists:enumerate(Microchips)},
    {compress(A, Map), compress(B, Map), compress(C, Map), compress(D, Map)}.

compress(Floor, Map) ->
    [{maps:get(E, Map), T} || {E, T} <- Floor].

get_floor(N, Floors) ->
    erlang:element(N, Floors).

select_floors(1) -> [2];
select_floors(2) -> [1, 3];
select_floors(3) -> [2, 4];
select_floors(4) -> [3].

select_items([]) ->
    [];
select_items([A]) ->
    [[A]];
select_items([A | Rest]) ->
    [[A] | [[A, R] || R <- Rest]] ++ select_items(Rest).
